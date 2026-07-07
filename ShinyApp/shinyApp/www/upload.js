// ---------------------------------------------------------------------------
// User upload progress UI bridge
// ---------------------------------------------------------------------------
// Keeps the custom upload progress panel synchronized with the Shiny file input
// and server-side processing state. This script is defensive because Shiny can
// recreate input elements during UI refreshes.
(function () {
  var statusId = "user-upload-status";
  var areaSelector = ".upload-progress-area";
  var progressId = "user_seurat_file_progress";
  var AREA_STATES = ["uploading", "processing", "complete", "error"];
  var resetTimer = null;

  // DOM lookups are wrapped because the upload UI may be absent on hidden tabs.
  function getStatusEl() {
    return document.getElementById(statusId);
  }

  function getAreaEl() {
    return document.querySelector(areaSelector);
  }

  function getDefaultMessage() {
    var statusEl = getStatusEl();
    return statusEl ? statusEl.getAttribute("data-default") || "" : "";
  }

  function setStatus(message, isActive) {
    var statusEl = getStatusEl();
    if (statusEl) {
      var text =
        typeof message === "string" && message.length
          ? message
          : getDefaultMessage();
      statusEl.textContent = text;
    }
    if (typeof isActive === "boolean") {
      var area = getAreaEl();
      if (area) {
        area.classList.toggle("active", isActive);
      }
    }
  }

  // Restore the idle message and remove visual state from the upload panel.
  function resetStatus() {
    setStatus(null, false);
    setAreaState(null);
  }

  // Keep state classes mutually exclusive so CSS can target one upload phase at
  // a time.
  function setAreaState(state) {
    var area = getAreaEl();
    if (!area) {
      return;
    }
    AREA_STATES.forEach(function (cls) {
      area.classList.remove(cls);
    });
    if (state && AREA_STATES.indexOf(state) !== -1) {
      area.classList.add(state);
    }
  }

  // Shiny creates its progress element outside the custom panel; move it into
  // the panel so the app can style upload progress consistently.
  function moveProgressBar() {
    var progress = document.getElementById(progressId);
    var area = getAreaEl();
    if (!progress || !area) {
      return;
    }
    progress.classList.add("upload-progress-track");
    if (!area.contains(progress)) {
      area.appendChild(progress);
    }
  }

  // Bind once per input element and mark the element to survive repeated init
  // calls after Shiny UI updates.
  function bindFileInputListener() {
    var input = document.getElementById("user_seurat_file");
    if (!input || input.__uploadListenerBound) {
      return;
    }
    input.addEventListener("change", function () {
      moveProgressBar();
      if (this.files && this.files.length) {
        clearTimeout(resetTimer);
        resetTimer = null;
        setAreaState("uploading");
        setStatus("Uploading Seurat object…", true);
      } else {
        resetStatus();
      }
    });
    input.__uploadListenerBound = true;
  }

  // Server messages distinguish upload transport from R-side object processing.
  function handleStatusMessage(msg) {
    if (!msg || !msg.state) {
      return;
    }
    clearTimeout(resetTimer);
    resetTimer = null;

    if (msg.state === "processing") {
      setAreaState("processing");
      setStatus(
        msg.message ||
          "Processing uploaded Seurat object… This can take several minutes.",
        true
      );
    } else if (msg.state === "uploading") {
      setAreaState("uploading");
      setStatus(msg.message || "Uploading Seurat object…", true);
    } else if (msg.state === "complete") {
      setAreaState("complete");
      setStatus(msg.message || "Upload finished.", true);
      resetTimer = setTimeout(resetStatus, 4500);
    } else if (msg.state === "error") {
      setAreaState("error");
      setStatus(msg.message || "Upload failed.", false);
    } else if (msg.state === "idle") {
      resetStatus();
    }
  }

  // Register after Shiny connects when this script loads before the client
  // runtime is ready.
  function registerShinyHandler() {
    if (!(window.Shiny && window.Shiny.addCustomMessageHandler)) {
      return false;
    }
    window.Shiny.addCustomMessageHandler(
      "user-upload-status",
      handleStatusMessage
    );
    return true;
  }

  // Initialize immediately when the DOM is ready, then keep hooks current when
  // Shiny replaces the file input.
  function init() {
    bindFileInputListener();
    moveProgressBar();
    resetStatus();
    document.addEventListener("shiny:inputchanged", function (ev) {
      if (ev.detail && ev.detail.name === "user_seurat_file") {
        bindFileInputListener();
        setTimeout(moveProgressBar, 0);
      }
    });
    if (!registerShinyHandler()) {
      document.addEventListener(
        "shiny:connected",
        function () {
          registerShinyHandler();
        },
        { once: true }
      );
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
