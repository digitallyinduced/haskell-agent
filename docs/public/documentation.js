(() => {
  "use strict";

  const root = document.documentElement;
  const searchInput = document.getElementById("search-input");
  if (searchInput) {
    searchInput.setAttribute("aria-keyshortcuts", "Control+k Meta+k");
    const shortcut = document.createElement("kbd");
    shortcut.className = "search-shortcut";
    shortcut.textContent = "Ctrl / ⌘ K";
    shortcut.setAttribute("aria-hidden", "true");
    searchInput.insertAdjacentElement("afterend", shortcut);
    document.addEventListener("keydown", (event) => {
      if ((event.ctrlKey || event.metaKey) && !event.altKey && !event.shiftKey &&
          !event.isComposing && event.key.toLowerCase() === "k") {
        event.preventDefault();
        searchInput.focus();
        searchInput.select();
      }
    });
  }
  const themeSelector = document.getElementById("theme-select");
  const acceptedThemes = new Set(["auto", "light", "dark"]);
  let selectedTheme = "auto";
  try {
    const savedTheme = localStorage.getItem("documentation-theme");
    if (acceptedThemes.has(savedTheme)) selectedTheme = savedTheme;
  } catch (_) {
    // Storage can be unavailable in restricted browser contexts.
  }
  root.dataset.theme = selectedTheme;
  if (themeSelector) {
    themeSelector.value = selectedTheme;
    themeSelector.addEventListener("change", () => {
      const theme = acceptedThemes.has(themeSelector.value) ? themeSelector.value : "auto";
      root.dataset.theme = theme;
      try { localStorage.setItem("documentation-theme", theme); } catch (_) {}
    });
  }

  const menuButton = document.getElementById("menu-toggle");
  const sidebar = document.getElementById("sidebar");
  if (menuButton && sidebar) {
    root.dataset.navigationReady = "";
    const closeMenu = () => {
      document.body.removeAttribute("data-mobile-menu-expanded");
      menuButton.setAttribute("aria-expanded", "false");
    };
    menuButton.addEventListener("click", () => {
      const expanded = menuButton.getAttribute("aria-expanded") !== "true";
      menuButton.setAttribute("aria-expanded", String(expanded));
      document.body.toggleAttribute("data-mobile-menu-expanded", expanded);
    });
    sidebar.addEventListener("click", (event) => {
      if (event.target.closest("a")) closeMenu();
    });
    document.addEventListener("keydown", (event) => {
      if (event.key === "Escape" && menuButton.getAttribute("aria-expanded") === "true") {
        closeMenu();
        menuButton.focus();
      }
    });
    window.matchMedia("(min-width: 761px)").addEventListener("change", closeMenu);
  }

  const article = document.querySelector("#main-content article");
  const contents = document.getElementById("table-of-contents");
  if (article && contents && !contents.querySelector("a")) {
    const headings = Array.from(article.querySelectorAll("h2, h3"));
    if (headings.length) {
      const title = document.createElement("h2");
      title.textContent = "On this page";
      const list = document.createElement("ul");
      headings.forEach((heading) => {
        if (!heading.id) {
          const base = heading.textContent.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "") || "section";
          let identifier = base;
          let suffix = 1;
          while (document.getElementById(identifier)) identifier = `${base}-${suffix++}`;
          heading.id = identifier;
        }
        const item = document.createElement("li");
        if (heading.tagName === "H3") item.className = "subsection";
        const link = document.createElement("a");
        link.href = `#${heading.id}`;
        link.textContent = heading.textContent;
        item.append(link);
        list.append(item);
      });
      contents.replaceChildren(title, list);
    }
  }

  // Deliberately small lexical highlighter, not a language parser. Only
  // explicitly labelled shell/JSON examples are enhanced. Text nodes preserve
  // every source character and never interpret a sample as HTML.
  if (article) {
    article.querySelectorAll("pre > code").forEach((code) => {
      const language = ["sh", "bash", "json"].find((name) => code.classList.contains(`language-${name}`));
      if (!language) return;
      const source = code.textContent;
      const tokens = language === "json"
        ? /("(?:\\.|[^"\\])*")|(\b(?:true|false|null)\b)|(-?\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)/g
        : /("(?:\\.|[^"\\])*"|'[^']*')|((?<!\S)#[^\n]*)|(\$\{[^}\n]+\}|\$[A-Za-z_][A-Za-z0-9_]*)|((?<!\S)--?[A-Za-z][A-Za-z0-9-]*)|(\b(?:nix|git|cd|mkdir|printf|export|curl|cat|echo)\b)/g;
      const fragment = document.createDocumentFragment();
      let cursor = 0;
      for (const match of source.matchAll(tokens)) {
        fragment.append(document.createTextNode(source.slice(cursor, match.index)));
        const token = document.createElement("span");
        const kind = language === "json"
          ? (match[1] ? (/^\s*:/.test(source.slice(match.index + match[0].length)) ? "key" : "string") : "literal")
          : (match[1] ? "string" : match[2] ? "comment" : match[3] ? "variable" : match[4] ? "option" : "command");
        token.className = `syntax-${kind}`;
        token.textContent = match[0];
        fragment.append(token);
        cursor = match.index + match[0].length;
      }
      fragment.append(document.createTextNode(source.slice(cursor)));
      code.replaceChildren(fragment);
      const label = document.createElement("span");
      label.className = "code-language";
      label.textContent = language === "json" ? "JSON" : "Shell";
      label.setAttribute("aria-hidden", "true");
      code.parentElement.prepend(label);
    });
  }

  if (article && navigator.clipboard && navigator.clipboard.writeText) {
    article.querySelectorAll("pre > code").forEach((code) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "copy-button";
      button.textContent = "Copy";
      button.setAttribute("aria-label", "Copy code");
      button.setAttribute("aria-live", "polite");
      button.addEventListener("click", async () => {
        try {
          await navigator.clipboard.writeText(code.textContent);
          button.textContent = "Copied";
          button.setAttribute("aria-label", "Code copied");
        } catch (_) {
          button.textContent = "Copy failed";
          button.setAttribute("aria-label", "Copy failed");
        }
        window.setTimeout(() => {
          button.textContent = "Copy";
          button.setAttribute("aria-label", "Copy code");
        }, 2000);
      });
      code.parentElement.append(button);
    });
  }
})();
