// Headless version of tbsl-fixtures-generator.js.
// Opens the TBF season page in Chromium (so the API call carries a real browser session /
// Cloudflare clearance), pulls every week's matches, and writes tbsl-fixtures.json.
// Run from the repo root:  node bball-stats/scrapers/fetch-fixtures.mjs
import { chromium } from "playwright";
import { readFileSync, writeFileSync, existsSync } from "fs";

const ACTIVITY_ID = process.env.ACTIVITY_ID ?? "22214";
const SEASON_URL  = process.env.SEASON_URL  ?? "https://www.tbf.org.tr/ligler/bsl-2026-2027";
const OUT         = "bball-stats/scrapers/tbsl-fixtures.json";

const browser = await chromium.launch();
const page = await browser.newPage({ locale: "tr-TR" });
await page.goto(SEASON_URL, { waitUntil: "networkidle" });
// If Cloudflare shows an interstitial, give it up to 30 s to clear.
await page
  .waitForFunction(() => !/just a moment|bir dakika/i.test(document.title), { timeout: 30_000 })
  .catch(() => {});

const fixtures = await page.evaluate(async (activityId) => {
  const out = [];
  for (let wk = 1; wk <= 40; wk++) {
    const r = await fetch(
      `/api/Match/get-all-matches-for-filter?ActivityId=${activityId}&WeekFilter=${wk}&Page=1&PageSize=-1`
    );
    if (!r.ok) throw new Error(`week ${wk}: HTTP ${r.status}`);
    const j = await r.json();
    if (!j.data || j.data.length === 0) break;
    for (const m of j.data) {
      out.push({
        geniusId: m.genuisId ?? "",   // TBF's spelling
        date: m.matchDateOnly,
        home: m.homeTeam.name,
        away: m.awayTeam.name,
      });
    }
  }
  return out;
}, ACTIVITY_ID);
await browser.close();

fixtures.sort((a, b) => a.date.localeCompare(b.date));
const withIds = fixtures.filter((f) => f.geniusId).length;

// Safety net: never replace a good file with a worse one (empty response, partial week, etc.).
if (existsSync(OUT)) {
  const prev = JSON.parse(readFileSync(OUT, "utf8"));
  const prevWithIds = prev.filter((f) => f.geniusId).length;
  if (fixtures.length < prev.length || withIds < prevWithIds) {
    console.error(`Refusing to overwrite: new ${fixtures.length}/${withIds} ids vs existing ${prev.length}/${prevWithIds}`);
    process.exit(1);
  }
}

writeFileSync(OUT, JSON.stringify(fixtures, null, 1) + "\n");
console.log(`DONE - ${fixtures.length} fixtures, ${withIds} with FLS ids`);
