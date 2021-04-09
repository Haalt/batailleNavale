(() => {
  const shipSizes = [5, 4, 3, 3, 2];
  const names = [
    "porte avion",
    "croiseur",
    "contre torpilleur",
    "contre torpilleur",
    "torpilleur",
  ];
  let isVertical = true;
  const ships = [];

  class Ship {
    constructor({ size, pos }) {
      this.size = size;
      this.name = names[0];
      this.pos = pos;
    }
  }

  const formatPlaceShipParameters = (shipArr) => {
    let already3 = false;
    let stringParam = "";
    const sizeMap = [3, 0, 5, 4, 2, 1];
    for (let i = 0; i < shipArr.length; i++) {
      let index = sizeMap[shipArr[i].size];
      if (shipArr[i].size === 3 && !already3) {
        index = sizeMap[0];
        already3 = true;
      }
      for (let y = 0; y < shipArr[i].pos.length; y++) {
        stringParam += `&${index}px${y + 1}=${shipArr[i].pos[y].x}&${index}py${
          y + 1
        }=${shipArr[i].pos[y].y}`;
      }
    }
    return stringParam;
  };

  const postShipPositions = async () => {
    const res = await fetch(
      `/cgi-bin/placeShips.d.byte?player=${getPlayerName()}&gameId=${
        data.gameId
      }${formatPlaceShipParameters(ships)}`
    );
    if (res.status === 200) {
      let json = await res.json();
      json = JSON.parse(json);
      renderBoard(json, data.gameId, getPlayerName());
    } else {
      // handle err
    }
  };

  const applyClickHandler = (gameId, playerName) => {
    [...document.querySelectorAll("#opp-board .empty")].forEach((el) => {
      el.addEventListener("click", async (e) => {
        const pos = e.target.getAttribute("data-js").split("-");
        const res = await fetch(
          `/cgi-bin/shootPos.d.byte?posX=${pos[0]}&posY=${pos[1]}&player=${playerName}&gameId=${gameId}`
        );
        if (res.status === 200) {
          let json = await res.json();
          json = JSON.parse(json);
          renderBoard(json, gameId, playerName);
        } else {
          // handle error
        }
      });
    });
  };

  const isEmpty = (mat) => {
    for (let x = 0; x < mat.length; x++)
      for (let y = 0; y < mat[x].length; y++)
        if (mat[x][y][0] !== "EMPTY") return false;
    return true;
  };

  const removeTmpShip = () => {
    [...document.querySelectorAll(".tmpShip")].forEach((el) =>
      el.classList.remove("tmpShip")
    );
  };

  const setupPlacementListener = () => {
    const clickHandler = (e) => {
      const coordinates = [...document.querySelectorAll(".tmpShip")].map(
        (el) => {
          el.classList.add("ship");
          const coo = el.getAttribute("data-js").split("-");
          return { x: coo[0], y: coo[1] };
        }
      );
      ships.push(
        new Ship({
          size: shipSizes[0],
          pos: coordinates,
          name: names[0],
        })
      );
      removeTmpShip();
      shipSizes.shift();
      names.shift();
      if (shipSizes.length === 0) {
        postShipPositions();
      }
    };

    [...document.querySelectorAll("#board td")].forEach((el) => {
      el.addEventListener("mouseenter", (e) => {
        const currentShip = shipSizes[0];
        const currentCase = e.target
          .getAttribute("data-js")
          .split("-")
          .map((el) => Number(el));
        for (
          let i = currentCase[isVertical ? 0 : 1];
          i < currentCase[isVertical ? 0 : 1] + currentShip;
          i++
        ) {
          try {
            const element = document.querySelector(
              `[data-js="${isVertical ? i : currentCase[0]}-${
                isVertical ? currentCase[1] : i
              }"]`
            );
            if (element.classList.contains("ship")) {
              removeTmpShip();
              return;
            }
            element.classList.add("tmpShip");
          } catch (e) {
            removeTmpShip();
            return;
          }
        }
        el.addEventListener("click", clickHandler);

        el.addEventListener("mouseleave", () => {
          el.removeEventListener("click", clickHandler);
          removeTmpShip();
        });
      });
    });
  };

  const renderShipPlacement = () => {
    document.querySelector("body").innerHTML +=
      '<div><button data-js="rotate">rotate</button></div>';
    document
      .querySelector("[data-js=rotate]")
      .addEventListener("click", () => (isVertical = !isVertical));
    setupPlacementListener();
  };

  const getPlayerName = () => {
    let playerName = document.cookie.split("=");
    return playerName[playerName.indexOf("pseudo") + 1];
  };

  const fetchGameData = async (gameId) => {
    let res = await fetch(
      `/cgi-bin/getGameData.d.byte?gameId=${gameId}&player=${getPlayerName()}`
    );
    if (res.status === 200) {
      let json = await res.json();
      json = JSON.parse(json);
      renderBoard(json, gameId, getPlayerName());
      if (isEmpty(json.player1.mymat)) {
        renderShipPlacement();
      }
    } else {
      // window.location.pathname = "/";
      // handle error
    }
  };

  const renderBoard = (p, gameId, playerName) => {
    const html = ["", ""];
    const htmlElements = [
      document.getElementById("board"),
      document.getElementById("opp-board"),
    ];
    for (let i = 0; i < html.length; i++) {
      for (let x = 0; x < p.prm.mat.dx; x++) {
        html[i] += "<tr>";
        for (let y = 0; y < p.prm.mat.dy; y++) {
          var thisPos =
            i === 0
              ? p.player1.mymat[x][y][0].toLowerCase()
              : p.player1.opp_mat[x][y][0].toLowerCase();
          html[i] += '<td class="';
          if (thisPos !== null) html[i] += thisPos;
          html[i] += `" data-js="${x}-${y}">`;
          html[i] += "</td>";
        }
        html[i] += "</tr>";
        console.log(html[i]);
      }
      htmlElements[i].innerHTML = html[i];
    }
    applyClickHandler(gameId, playerName);
  };

  createBoard = () => {
    document.querySelector("body").innerHTML =
      '<div id="battleship"><span id="result"></span><table id="board"></table><table id="opp-board"></table></div>';
  };

  createBoard();
  const data = {};
  const cleanQuery = window.location.search
    .replace("?", "")
    .split("&")
    .map((s) => [s.split("=")[0], s.split("=")[1]]);
  cleanQuery.forEach((el) => (data[el[0]] = el[1]));
  console.log(data);
  fetchGameData(data.gameId, data.pseudo);
})();
