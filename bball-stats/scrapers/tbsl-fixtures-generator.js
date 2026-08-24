// ================================================================================================
// TBSL fixtures downloader - run ONCE PER SEASON (30 seconds).
// Paste into the browser console (F12 > Console) while on
// https://www.tbf.org.tr/ligler/bsl-2025-2026 and press Enter.
// (If Chrome blocks pasting, type "allow pasting" in the console first.)
//
// Runs INSIDE your browser, so it uses your existing Cloudflare clearance - no cookie handling.
// Saves tbsl-fixtures.json (every game of the season: FLS id, date, teams). Put that file next
// to TR-scraper.R. For a new season, change ACTIVITY_ID (grab it from the Network tab).
// ================================================================================================
(async () => {
  const ACTIVITY_ID = 20728; // BSL 2025-2026

  const fixtures = [];
  for (let wk = 1; wk <= 40; wk++) {
    const r = await fetch(`/api/Match/get-all-matches-for-filter?ActivityId=${ACTIVITY_ID}&WeekFilter=${wk}&Page=1&PageSize=-1`);
    const j = await r.json();
    if (!j.data || j.data.length === 0) break;
    for (const m of j.data) {
      fixtures.push({
        geniusId: m.genuisId,          // FLS match id (note TBF's spelling "genuisId")
        date: m.matchDateOnly,         // YYYY-MM-DD
        home: m.homeTeam.name,
        away: m.awayTeam.name
      });
    }
    console.log(`week ${wk}: ${j.data.length} games`);
  }
  fixtures.sort((a, b) => a.date.localeCompare(b.date));

  const blob = new Blob([JSON.stringify(fixtures, null, 1)], { type: "application/json" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = "tbsl-fixtures.json";
  a.click();
  console.log(`DONE - ${fixtures.length} fixtures saved to tbsl-fixtures.json`);
})();
