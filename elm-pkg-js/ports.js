exports.init = async function (app) {
  app.ports.set_local_storage_to_js.subscribe(function (data) {
    window.localStorage.setItem("a", JSON.stringify(data));
  });

  app.ports.get_local_storage_to_js.subscribe(function (data) {
    let localStorageValue = window.localStorage.getItem("a");
    if (localStorageValue !== null) {
      app.ports.got_local_storage_from_js.send(JSON.parse(localStorageValue));
    }
    else {
      app.ports.got_local_storage_from_js.send(null);
    }
  });
}