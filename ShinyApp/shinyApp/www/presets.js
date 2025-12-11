;(function() {
  function fallbackCopy(text) {
    var textarea = document.createElement("textarea");
    textarea.value = text;
    textarea.setAttribute("readonly", "");
    textarea.style.position = "absolute";
    textarea.style.left = "-9999px";
    document.body.appendChild(textarea);
    textarea.select();
    try {
      document.execCommand("copy");
    } catch (err) {
      console.warn("Clipboard copy failed:", err);
    }
    document.body.removeChild(textarea);
  }

  function notify(message) {
    if (window.Shiny && typeof Shiny.setInputValue === "function") {
      Shiny.setInputValue("preset_copy_notice", { time: Date.now(), message: message }, { priority: "event" });
    }
  }

  Shiny.addCustomMessageHandler("copy-to-clipboard", function(payload) {
    var text = payload && payload.text;
    if (!text) { return; }
    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      navigator.clipboard.writeText(text)
        .then(function() { notify("Sharable link copied to clipboard"); })
        .catch(function() {
          fallbackCopy(text);
          notify("Sharable link copied");
        });
    } else {
      fallbackCopy(text);
      notify("Sharable link copied");
    }
  });
})();
