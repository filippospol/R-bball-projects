// Extracts the BBL public API secret (the x-api-secret header the easycredit-bbl.de frontend
// sends to api.basketball-bundesliga.de) so DE-scraper.R no longer depends on a hand-copied
// GitHub secret. Two strategies:
//   1. open the site in headless Chromium and read the header off the first API request
//   2. fallback: grep the site's Next.js JS chunks for the header value
// Prints the secret on stdout (nothing else) so the workflow can capture and mask it.
import { chromium } from "playwright";

const SITE = "https://www.easycredit-bbl.de/saison/aktuelle-spiele";
const API_HOST = "api.basketball-bundesliga.de";

async function viaInterception() {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({ locale: "de-DE" });
    const found = new Promise((resolve) => {
      page.on("request", (req) => {
        if (!req.url().includes(API_HOST)) return;
        const h = req.headers();
        const secret = h["x-api-secret"] ?? h["X-Api-Secret"];
        if (secret) resolve(secret);
      });
    });
    await page.goto(SITE, { waitUntil: "domcontentloaded", timeout: 60_000 });
    return await Promise.race([
      found,
      new Promise((_, rej) => setTimeout(() => rej(new Error("no API request seen in 45 s")), 45_000)),
    ]);
  } finally {
    await browser.close();
  }
}

async function viaBundles() {
  const ua = { "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/124.0 Safari/537.36" };
  const html = await (await fetch(SITE, { headers: ua })).text();
  const chunks = [...new Set([...html.matchAll(/["'](\/_next\/static\/[^"']+\.js)["']/g)].map((m) => m[1]))];
  const patterns = [
    /["']?x-api-secret["']?\s*[:=]\s*["']([A-Za-z0-9._\-]{8,})["']/i,
    /X-Api-Secret["']?\s*[:=]\s*["']([A-Za-z0-9._\-]{8,})["']/,
  ];
  for (const c of chunks) {
    const js = await (await fetch(`https://www.easycredit-bbl.de${c}`, { headers: ua })).text();
    for (const p of patterns) {
      const m = js.match(p);
      if (m) return m[1];
    }
  }
  throw new Error(`secret not found in ${chunks.length} JS chunks`);
}

let secret;
try {
  secret = await viaInterception();
  console.error("secret captured from a live API request");
} catch (e) {
  console.error(`interception failed (${e.message}); trying JS bundles...`);
  secret = await viaBundles();
  console.error("secret found in JS bundle");
}
process.stdout.write(secret);
