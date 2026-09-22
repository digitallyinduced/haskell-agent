"""Exercise a built documentation site with Playwright and a local Chrome binary.

Supply DOCUMENTATION_URL, CHROME_EXECUTABLE, and DOCUMENTATION_SCREENSHOTS.
The browser uses a fresh temporary profile; no personal browser data is read.
"""

import os
import json
import re
from pathlib import Path
from playwright.sync_api import sync_playwright, expect


address = os.environ.get("DOCUMENTATION_URL", "http://127.0.0.1:4321")
screenshots = Path(os.environ["DOCUMENTATION_SCREENSHOTS"])
screenshots.mkdir(parents=True, exist_ok=True)

with sync_playwright() as playwright:
    browser = playwright.chromium.launch(
        executable_path=os.environ["CHROME_EXECUTABLE"],
        chromium_sandbox=True,
    )
    try:
        context = browser.new_context(viewport={"width": 1440, "height": 1000})
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        response = page.goto(address + "/")
        assert response.status == 200
        expect(page.locator("h1")).to_be_visible()
        documentation_paths = page.locator("#sidebar .navigation-group a").evaluate_all(
            "links => links.map(link => link.getAttribute('href'))")
        page.screenshot(path=str(screenshots / "documentation-desktop.png"))
        page.goto(address + "/getting-started/installation/")
        expect(page.locator("h1")).to_be_visible()
        expect(page.locator(".text-export")).to_have_attribute(
            "href", "/text/getting-started/installation/")
        expect(page.locator("#table-of-contents a").first).to_be_visible()
        expect(page.locator("code.language-sh .syntax-command").first).to_be_visible()
        original_examples = page.evaluate("""async () => {
          const html = await (await fetch(location.href)).text();
          const document = new DOMParser().parseFromString(html, 'text/html');
          return Array.from(document.querySelectorAll('pre > code'), code => code.textContent);
        }""")
        assert page.locator("pre > code").all_text_contents() == original_examples
        # Capture writes from the actual copy button without reading the host clipboard.
        page.evaluate("""() => {
          window.copiedDocumentation = null;
          Object.defineProperty(navigator.clipboard, 'writeText', {
            value: async (text) => { window.copiedDocumentation = text; }
          });
        }""")
        page.locator(".copy-button").first.click()
        page.wait_for_function("window.copiedDocumentation?.includes('nix')")
        assert page.evaluate("window.copiedDocumentation") == original_examples[0]
        theme = page.locator("#theme-select")
        theme.select_option("dark")
        expect(page.locator("html")).to_have_attribute("data-theme", "dark")
        page.reload()
        expect(page.locator("html")).to_have_attribute("data-theme", "dark")
        theme.select_option("light")
        expect(page.locator("html")).to_have_attribute("data-theme", "light")
        page.screenshot(path=str(screenshots / "documentation-installation-light.png"))
        page.locator("h1").click()
        page.keyboard.press("Control+k")
        expect(page.locator("#search-input")).to_be_focused()
        page.locator("h1").click()
        page.keyboard.press("Meta+k")
        expect(page.locator("#search-input")).to_be_focused()
        expect(page.get_by_role("navigation", name="Adjacent pages")).to_be_visible()
        expect(page.get_by_role("link", name="Report a documentation issue")).to_have_attribute(
            "href", "https://github.com/digitallyinduced/haskell-agent/issues/new")
        page.locator("#search-input").fill("sessions")
        page.locator("#search-input").press("Enter")
        results = page.locator(".search-results a")
        expect(results.first).to_be_visible()
        page.screenshot(path=str(screenshots / "documentation-search.png"))
        results.first.click()
        expect(page).to_have_url(address + "/guides/sessions/")
        expect(page.locator("h1")).to_be_visible()
        page.set_viewport_size({"width": 390, "height": 844})
        json_examples = 0
        for route in documentation_paths:
            response = page.goto(address + route)
            assert response.status == 200
            expect(page.locator("h1")).to_be_visible()
            assert page.evaluate("document.documentElement.scrollWidth <= window.innerWidth"), route
            for illustration in page.locator("article img").all():
                expect(illustration).to_have_attribute("alt", re.compile(r".+"))
                illustration.scroll_into_view_if_needed()
                page.wait_for_function("Array.from(document.querySelectorAll('article img')).every(image => image.complete && image.naturalWidth > 0)")
            for example in page.locator("pre > code.language-json").all_text_contents():
                json.loads(example)
                json_examples += 1
            headings = page.locator("article h2[id], article h3[id]").evaluate_all(
                "headings => headings.map(heading => '#' + heading.id)")
            contents = page.locator("#table-of-contents a").evaluate_all(
                "links => links.map(link => link.getAttribute('href'))")
            assert contents == headings, route
        assert json_examples > 0
        page.goto(address + "/reference/configuration/")
        page.locator(".mobile-contents summary").click()
        expect(page.locator(".mobile-contents a").first).to_be_visible()
        page.screenshot(path=str(screenshots / "documentation-configuration-mobile.png"))
        page.goto(address + "/")
        page.screenshot(path=str(screenshots / "documentation-mobile.png"))
        assert page.evaluate("document.documentElement.scrollWidth <= window.innerWidth")
        menu = page.locator("#menu-toggle")
        menu.click()
        expect(menu).to_have_attribute("aria-expanded", "true")
        page.keyboard.press("Escape")
        expect(menu).to_have_attribute("aria-expanded", "false")
        menu.click()
        page.locator("#sidebar a[href='/guides/sessions/']").click()
        expect(page).to_have_url(address + "/guides/sessions/")
        expect(menu).to_have_attribute("aria-expanded", "false")
        assert page.evaluate("document.documentElement.scrollWidth <= window.innerWidth")
        # The application must remain navigable and searchable without JavaScript.
        unenhanced = browser.new_context(
            java_script_enabled=False, viewport={"width": 390, "height": 844})
        unenhanced_page = unenhanced.new_page()
        unenhanced_page.goto(address + "/")
        unenhanced_page.locator(".mobile-contents summary").click()
        expect(unenhanced_page.locator(".mobile-contents a").first).to_be_visible()
        unenhanced_page.locator("#sidebar a[href='/guides/sessions/']").click()
        expect(unenhanced_page).to_have_url(address + "/guides/sessions/")
        unenhanced_page.locator("#search-input").fill("skills")
        unenhanced_page.locator("#search-input").press("Enter")
        expect(unenhanced_page.locator(".search-results a").first).to_be_visible()
        unenhanced.close()
        assert not errors, errors
        print(f"Browser checks passed: {len(documentation_paths)} pages, {json_examples} JSON examples, mobile overflow, section navigation, server search, themes, copy button, text export, and JavaScript-disabled use.")
    finally:
        browser.close()
