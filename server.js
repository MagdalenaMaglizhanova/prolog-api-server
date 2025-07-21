const express = require("express");
const { execFile } = require("child_process");
const path = require("path");
const app = express();
const port = process.env.PORT || 3000;

app.use(express.json());

app.post("/prolog", (req, res) => {
  const { query } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });

  const prologFile = path.join(__dirname, "prolog_files", "example1.pl");
  const hasVars = /[A-Z]/.test(query);

  let goal = "";

  if (hasVars) {
    const argsMatch = query.match(/\((.*)\)/);
    const args = argsMatch ? argsMatch[1] : "";
    goal = `findall([${args}], ${query}, L), writeq(L), nl, halt.`;
  } else {
    goal = `${query}, write('true'), nl, halt.`;
  }

  execFile("swipl", ["-q", "-s", prologFile, "-g", goal], (error, stdout, stderr) => {
    if (error) {
      res.status(500).json({ error: stderr || error.message });
    } else {
      res.json({ result: stdout.trim() || "false" });
    }
  });
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
