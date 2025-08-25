const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");

const app = express();
const port = process.env.PORT || 10000;

app.use(cors());
app.use(express.json());

// Дефинирайте mapping между стойностите от менюто и имената на файловете
const knowledgeBaseMap = {
  mineral_waters: "mineral_water.pl", // Стойността от <option value="mineral_waters">
  history: "history.pl"               // Стойността от <option value="history">
};

app.post("/prolog", (req, res) => {
  const { query, knowledgeBase } = req.body;

  if (!query) {
    return res.status(400).json({ error: "No query provided" });
  }

  // 1. ВЗЕМИ ПРАВИЛНИЯ ФАЙЛ ВЪЗ ОСНОВА НА ИЗБОРА ОТ МЕНЮТО
  const prologFileName = knowledgeBaseMap[knowledgeBase];
  if (!prologFileName) {
    return res.status(400).json({ error: "Invalid knowledge base selected" });
  }

  const prologFile = path.join(__dirname, "prolog_files", prologFileName);

  const hasVars = /[A-Z]/.test(query);
  let goal = "";

  if (hasVars) {
    // НОВ КОД: Филтрираме само релевантните променливи (започващи с главна буква)
    const variableRegex = /[A-Z][a-zA-Z0-9_]*/g;
    const variables = query.match(variableRegex);
    const args = variables ? variables.join(',') : "";
    
    goal = `findall([${args}], ${query}, L), writeq(L), nl, halt.`;
  } else {
    goal = `${query}, write('true'), nl, halt.`;
  }

  // 2. СТАРТИРАЙ SWI-PROLOG САМО С ИЗБРАНИЯ ФАЙЛ
  execFile("swipl", ["-q", "-s", prologFile, "-g", goal], (error, stdout, stderr) => {
    if (error) {
      console.error("Prolog Error:", error);
      console.error("Prolog Stderr:", stderr);
      res.status(500).json({ error: stderr || error.message });
    } else {
      res.json({ result: stdout.trim() || "false" });
    }
  });
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
