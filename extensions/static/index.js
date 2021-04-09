(() => {
  const setCookie = (name, value) => {
    const date = new Date();
    date.setTime(date.getTime() + 360 * 24 * 60 * 60 * 1000);
    const expires = "; expires=" + date.toUTCString();
    document.cookie = name + "=" + (value || "") + expires + "; path=/";
  };
  const displayCreateGame = () => {
    document.querySelector("body").innerHTML =
      '<div id="createForm"><label for="pseudo1">Pseudo</label><input id="pseudo1" type="text"><label for="player">Pesudo</label><select id="player"><option value="1" selected>1 (vs computer)</option><option value="2">2 (multiplayer)</option></select><button id="create">Create game</button></div>';
    document.querySelector("#player").addEventListener("change", (e) => {
      if (e.target.value == "1") {
        document.querySelector("#pseudo2").remove();
        document.querySelector("#label2").remove();
      } else {
        const label = document.createElement("label");
        label.htmlFor = "#pseudo2";
        label.id = "label2";
        label.textContent = "Pseudo 2";
        const input = document.createElement("input");
        input.id = "pseudo2";
        let referenceNode = document.querySelector("#pseudo1");
        referenceNode.parentNode.insertBefore(label, referenceNode.nextSibling);
        referenceNode = document.querySelector("#label2");
        referenceNode.parentNode.insertBefore(input, referenceNode.nextSibling);
      }
    });

    document.querySelector("#create").addEventListener("click", async (e) => {
      const pseudo = document.querySelector("#pseudo1").value;
      const player = document.querySelector("#player").value;
      if (pseudo === "") return;
      let uri = `/cgi-bin/createGame.d.byte?pseudo=${pseudo}&player=${player}`;
      let pseudo2;
      if (player == "2") {
        pseudo2 = document.querySelector("#pseudo2").value;
        uri += `&pseudo2=${pseudo2}`;
      }
      const res = await fetch(uri);
      let json = await res.json();
      json = JSON.parse(json);
      setCookie("pseudo", pseudo);
      if (res.status === 200) {
        window.location.href = `/game?gameId=${json.gameId}`;
      }
    });
  };

  displayCreateGame();
})();
