function loadJSONNoPass(key) {
    const val = localStorage.getItem(key);
    if (val == null) {
        return ""
    } else {
        return val
    }
}

function saveJSONNoPass(key, val) {
    localStorage.setItem(key, val);
}
