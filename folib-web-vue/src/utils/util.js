export function timeFix() {
  const time = new Date();
  const hour = time.getHours();
  return hour < 9 ? "早上好" : hour <= 11 ? "上午好" : hour <= 13 ? "中午好" : hour < 20 ? "下午好" : "晚上好";
}

export function welcome() {
  const arr = ["休息一会儿吧", "准备吃什么呢?", "要不要打一把 DOTA", "我猜你可能累了"];
  const index = Math.floor(Math.random() * arr.length);
  return arr[index];
}

/**
 * 触发 window.resize
 */
export function triggerWindowResizeEvent() {
  const event = document.createEvent("HTMLEvents");
  event.initEvent("resize", true, true);
  event.eventType = "message";
  window.dispatchEvent(event);
}

export function handleScrollHeader(callback) {
  let timer = 0;

  let beforeScrollTop = window.pageYOffset;
  callback = callback || function () {};
  window.addEventListener(
    "scroll",
    (event) => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        let direction = "up";
        const afterScrollTop = window.pageYOffset;
        const delta = afterScrollTop - beforeScrollTop;
        if (delta === 0) {
          return false;
        }
        direction = delta > 0 ? "down" : "up";
        callback(direction);
        beforeScrollTop = afterScrollTop;
      }, 50);
    },
    false
  );
}

export function isIE() {
  const bw = window.navigator.userAgent;
  const compare = (s) => bw.indexOf(s) >= 0;
  const ie11 = (() => "ActiveXObject" in window)();
  return compare("MSIE") || ie11;
}

/**
 * Remove loading animate
 * @param id parent element id or class
 * @param timeout
 */
export function removeLoadingAnimate(id = "", timeout = 1500) {
  if (id === "") {
    return;
  }
  setTimeout(() => {
    document.body.removeChild(document.getElementById(id));
  }, timeout);
}
export function scorePassword(pass) {
  let score = 0;
  if (!pass) {
    return score;
  }
  // award every unique letter until 5 repetitions
  const letters = {};
  for (let i = 0; i < pass.length; i++) {
    letters[pass[i]] = (letters[pass[i]] || 0) + 1;
    score += 5.0 / letters[pass[i]];
  }

  // bonus points for mixing it up
  const variations = {
    digits: /\d/.test(pass),
    lower: /[a-z]/.test(pass),
    upper: /[A-Z]/.test(pass),
    nonWords: /\W/.test(pass),
  };

  let variationCount = 0;
  for (var check in variations) {
    variationCount += variations[check] === true ? 1 : 0;
  }
  score += (variationCount - 1) * 10;

  return parseInt(score);
}

export function formatTimestamp(timestamp, includeTime) {
  let date = new Date(timestamp);
  let months = ["1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"];
  function pad(num) {
    return num < 10 ? "0" + num : num;
  }
  if (includeTime) {
    return (
      date.getFullYear() +
      "年 " +
      months[date.getMonth()] +
      date.getDay() +
      "日 " +
      pad(date.getHours()) +
      ":" +
      pad(date.getMinutes()) +
      ":" +
      pad(date.getSeconds())
    );
  } else {
    return date.getFullYear() + "年 " + months[date.getMonth()];
  }
}

export function calcProgressPercent(total, completed) {
  if (total > 0) {
    if (completed === 0) {
      return 0;
    } else {
      let percentage = (completed / total) * 100;
      return Math.round(percentage);
    }
  } else if (completed > total) {
    // In something has already been completed (e.g. suppressed) and the completed value
    // is greater than the total, return 100%
    return 100;
  }
  return 0; // the absence of work does not imply progress.
}

export function valueWithDefault(variable, defaultValue) {
  if (variable) {
    return variable;
  } else {
    return defaultValue;
  }
}

export function trimToNull(value) {
  if (typeof value === "undefined") {
    return null;
  } else if (typeof value === "string" && value.trim() === "") {
    return null;
  }
  return value;
}

export function concatenateComponentName(group, name, version) {
  let g = trimToNull(group);
  let n = trimToNull(name);
  let v = trimToNull(version);
  return (g != null ? g + " " : "") + (n != null ? n : "") + (v != null ? " " + v : "");
}
