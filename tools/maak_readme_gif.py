"""Neemt een demo van het dashboard op en zet die om naar man/figures/demo.gif.

Maakt synthetische 1CHO- en VAKHAVW-bestanden, start de app uit inst/app,
uploadt de bestanden en loopt langs alle tabbladen, een vakkeuze, een filter
en de downloadknop. Opnieuw draaien na een UI-wijziging:

    uv run --no-project --with playwright python -m playwright install chromium   # eenmalig
    uv run --no-project --with playwright python tools/maak_readme_gif.py

Vereist R met staat1cho en de dashboardpackages, en ffmpeg op het PATH. De app
gebruikt de geinstalleerde staat1cho: installeer eerst de huidige versie.
"""

import shutil
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

from playwright.sync_api import TimeoutError, sync_playwright

ROOT = Path(__file__).resolve().parent.parent
UIT = ROOT / "man" / "figures" / "demo.gif"
POORT = 8767
BREEDTE, HOOGTE = 1280, 800
GIF_BREEDTE, FPS = 800, 10

# Synthetische data. De cohortgrootte varieert per jaar, anders is de
# instroomgrafiek een vlakke lijn. De VAKHAVW-cijfers zijn verzonnen voor de
# havo/vwo-studenten uit de synthetische 1CHO.
MAAK_DATA_R = """
library(staat1cho)
args <- commandArgs(trailingOnly = TRUE)
s <- maak_synthetische_1cho()
w <- attr(s, "waarheid")
set.seed(2)
aandeel <- setNames(runif(length(unique(w$instroomjaar)), 0.7, 1), sort(unique(w$instroomjaar)))
houd <- w$persoonsgebonden_nummer[runif(nrow(w)) < aandeel[as.character(w$instroomjaar)]]
s <- s[s$persoonsgebonden_nummer %in% houd, ]
hv <- w[w$vooropleiding %in% c("havo", "vwo") & w$persoonsgebonden_nummer %in% houd, ]
v <- do.call(rbind, lapply(c("ne", "en", "wis", "gs", "bi"), function(vak) {
  se <- round(rnorm(nrow(hv), 68, 7))
  ce <- round(rnorm(nrow(hv), 64, 9))
  data.frame(
    persoonsgebonden_nummer = hv$persoonsgebonden_nummer,
    gemiddeld_cijfer_cijferlijst = round((se + ce) / 2),
    afkorting_vak = vak,
    cijfer_schoolexamen = se,
    cijfer_eerste_centraal_examen = ce
  )
}))
readr::write_delim(s, args[1], delim = ";", na = "")
readr::write_delim(v, args[2], delim = ";", na = "")
"""

# Playwright neemt de muis niet op; deze stip maakt klikken zichtbaar.
CURSOR_JS = """
window.addEventListener('DOMContentLoaded', () => {
  const c = document.createElement('div');
  c.style.cssText = 'position:fixed;z-index:99999;width:22px;height:22px;'
    + 'margin:-11px 0 0 -11px;border-radius:50%;pointer-events:none;'
    + 'background:rgba(220,38,38,.35);border:2px solid rgba(220,38,38,.9);'
    + 'left:-50px;top:-50px;transition:transform .15s';
  document.body.appendChild(c);
  document.addEventListener('mousemove', e => {
    c.style.left = e.clientX + 'px'; c.style.top = e.clientY + 'px';
  });
  document.addEventListener('mousedown', () => c.style.transform = 'scale(.6)');
  document.addEventListener('mouseup', () => c.style.transform = '');
});
"""

# Klaar als niets in het actieve tabblad meer herberekent en elke grafiek er
# echte datapunten heeft (of er een tabel met rijen staat). Outputs in
# verborgen tabbladen houden .recalculating tot je ze opent, dus alleen het
# actieve tabblad telt.
INHOUD_JS = """() => {
  const tab = document.querySelector('.tab-pane.active');
  if (!tab || tab.querySelector('.recalculating')) return false;
  const plots = [...tab.querySelectorAll('.js-plotly-plot')];
  const rijen = tab.querySelectorAll('tbody tr td').length;
  if (!plots.length && rijen < 2) return false;  // DT toont eerst één lege cel
  return plots.every(p => p.querySelector('.plot .trace, .plot .point'));
}"""


def wacht_op_app(url, timeout=120):
    eind = time.time() + timeout
    while time.time() < eind:
        try:
            urllib.request.urlopen(url, timeout=2)
            return
        except OSError:
            time.sleep(0.5)
    sys.exit(f"App reageert niet op {url}")


def beweeg_naar(pagina, doel, pauze=500):
    doel.scroll_into_view_if_needed()
    box = doel.bounding_box()
    pagina.mouse.move(box["x"] + box["width"] / 2, box["y"] + box["height"] / 2, steps=20)
    pagina.wait_for_timeout(pauze)


def klik(pagina, selector, pauze=500):
    """Klik met de echte muis, zodat de cursorstip meebeweegt."""
    beweeg_naar(pagina, pagina.locator(selector).locator("visible=true").first, pauze)
    pagina.mouse.down()
    pagina.mouse.up()


def kies(pagina, invoer_id, optie):
    """Kies een optie in een selectize-dropdown van Shiny."""
    klik(pagina, f"#{invoer_id}-selectized")
    pagina.wait_for_timeout(400)
    klik(pagina, f".selectize-dropdown-content .option:text-is('{optie}')", pauze=400)


class Laadtijd:
    """Houdt bij wanneer de app aan het rekenen is; die stukken knipt ffmpeg eruit."""

    def __init__(self, start):
        self.start = start
        self.stukken = []

    def wacht(self, pagina, marge=0.15, timeout=20000, herberekent=False):
        van = time.time() - self.start + marge  # de klik zelf blijft zichtbaar
        if herberekent:
            # Na een filter zet Shiny .recalculating pas na een rondje naar de
            # server; wacht daarop, anders is de check te vroeg tevreden.
            try:
                pagina.wait_for_selector(".tab-pane.active .recalculating", timeout=3000)
            except TimeoutError:
                pass
        else:
            pagina.wait_for_timeout(200)
        try:
            pagina.wait_for_function(INHOUD_JS, timeout=timeout)
        except TimeoutError:
            # Bijv. een lege grafiek zonder datapunten: niet knippen
            print("  let op: inhoud niet herkend, niet geknipt", flush=True)
            return
        pagina.wait_for_timeout(300)  # grafiek tekenen
        tot = time.time() - self.start - 0.2
        if tot > van:
            self.stukken.append((van, tot))


def tab(pagina, laadtijd, naam, wacht=2000):
    print(f"  tabblad {naam}", flush=True)
    klik(pagina, f".nav-link:text-is('{naam}')")
    laadtijd.wacht(pagina)
    pagina.wait_for_timeout(wacht)


def neem_op(url, map_, cho, vakhawv):
    with sync_playwright() as p:
        browser = p.chromium.launch()
        context = browser.new_context(
            viewport={"width": BREEDTE, "height": HOOGTE},
            record_video_dir=map_,
            record_video_size={"width": BREEDTE, "height": HOOGTE},
            accept_downloads=True,
        )
        context.add_init_script(CURSOR_JS)
        pagina = context.new_page()
        start = time.time()
        laadtijd = Laadtijd(start)
        pagina.goto(url)
        bestanden = pagina.locator("input[type=file]")
        bestanden.first.wait_for(state="attached")
        pagina.wait_for_timeout(1000)
        begin = time.time() - start  # alles hiervoor is een leeg scherm

        # Uploadscherm: bestanden kiezen en verwerken
        print("  uploaden", flush=True)
        pagina.mouse.move(640, 300)
        beweeg_naar(pagina, pagina.locator(".btn-file").nth(0))
        bestanden.nth(0).set_input_files(cho)
        pagina.wait_for_timeout(1200)
        beweeg_naar(pagina, pagina.locator(".btn-file").nth(1))
        bestanden.nth(1).set_input_files(vakhawv)
        pagina.wait_for_timeout(1200)
        klik(pagina, "#btn_verwerk")
        laadtijd.wacht(pagina, marge=1.5, timeout=180000)
        pagina.wait_for_timeout(2500)

        for naam in ["Instroom", "Rendement", "Uitval", "Studiewissel"]:
            tab(pagina, laadtijd, naam)

        tab(pagina, laadtijd, "Vooropleiding", wacht=1200)
        kies(pagina, "vak_keuze", "en")
        laadtijd.wacht(pagina, herberekent=True)
        pagina.wait_for_timeout(2000)
        tab(pagina, laadtijd, "Data", wacht=1500)

        # Filter op sector: de cijfers in Overzicht passen zich aan
        tab(pagina, laadtijd, "Overzicht", wacht=1000)
        kies(pagina, "sector_filter", "techniek")
        laadtijd.wacht(pagina, herberekent=True)
        pagina.wait_for_timeout(2500)

        with pagina.expect_download(timeout=120000):
            klik(pagina, "#download_benchmark")
        pagina.wait_for_timeout(1500)

        video = pagina.video.path()
        context.close()
        browser.close()
    return Path(video), begin, laadtijd.stukken


def naar_gif(video, begin, weg):
    UIT.parent.mkdir(parents=True, exist_ok=True)
    filters = f"fps={FPS},scale={GIF_BREEDTE}:-1:flags=lanczos"
    if weg:
        print("  knippen:", ", ".join(f"{a - begin:.1f}-{b - begin:.1f}s" for a, b in weg))
        # Met -ss voor -i begint t bij 0 op het moment begin: trek dat af.
        knip = "+".join(f"between(t,{a - begin:.2f},{b - begin:.2f})" for a, b in weg)
        filters = f"select='not({knip})',setpts=N/FRAME_RATE/TB,{filters}"
    palet = video.with_suffix(".png")
    invoer = ["ffmpeg", "-v", "error", "-y", "-ss", f"{begin:.2f}", "-i", str(video)]
    subprocess.run(
        [*invoer, "-vf", f"{filters},palettegen=stats_mode=diff", str(palet)],
        check=True,
    )
    subprocess.run(
        [
            *invoer,
            "-i",
            str(palet),
            "-lavfi",
            f"{filters}[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle",
            str(UIT),
        ],
        check=True,
    )


def main():
    for programma in ("ffmpeg", "Rscript"):
        if not shutil.which(programma):
            sys.exit(f"{programma} niet gevonden op het PATH")
    url = f"http://127.0.0.1:{POORT}/"
    with tempfile.TemporaryDirectory() as map_:
        map_ = Path(map_)
        cho, vakhawv = map_ / "synth_1cho.csv", map_ / "synth_vakhawv.csv"
        script = map_ / "maak_data.R"
        script.write_text(MAAK_DATA_R, encoding="utf-8")
        subprocess.run(["Rscript", str(script), str(cho), str(vakhawv)], check=True)
        app = subprocess.Popen(
            [
                "Rscript",
                "-e",
                f"shiny::runApp('inst/app', port = {POORT}, launch.browser = FALSE)",
            ],
            cwd=ROOT,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        try:
            wacht_op_app(url)
            video, begin, weg = neem_op(url, str(map_), str(cho), str(vakhawv))
            naar_gif(video, begin, weg)
        finally:
            # Rscript kan een shim zijn (scoop) die R als kindproces start:
            # stop de hele boom, anders blijft de app op de poort draaien.
            if sys.platform == "win32":
                subprocess.run(
                    ["taskkill", "/F", "/T", "/PID", str(app.pid)], capture_output=True
                )
            else:
                app.terminate()
    print(f"{UIT.relative_to(ROOT)}: {UIT.stat().st_size / 1e6:.1f} MB")


if __name__ == "__main__":
    main()
