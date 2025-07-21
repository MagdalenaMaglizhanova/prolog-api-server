const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const os = require("os");

const app = express();
const port = 1000; // директно зададен порт 1000

app.use(cors());
app.use(express.json());

// Разрешени Prolog файлове за сигурност
const allowedFiles = ["example1.pl", "mineral_water.pl", "history.pl"];

app.post("/prolog", async (req, res) => {
  const { query, file, userCode } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!file) return res.status(400).json({ error: "No file specified" });
  if (!allowedFiles.includes(file)) return res.status(400).json({ error: "File not allowed" });

  // Път до основния Prolog файл
  const prologFile = path.join(__dirname, "prolog_files", file);

  // Функция за изпълнение на Prolog процес
  const runProlog = (consultFiles) => {
    const hasVars = /[A-Z]/.test(query);

    let goal = "";
    if (hasVars) {
      const argsMatch = query.match(/\((.*)\)/);
      const args = argsMatch ? argsMatch[1] : "";
      goal = `findall([${args}], (${query}), L), writeq(L), nl, halt.`;
    } else {
      goal = `${query}, write('true'), nl, halt.`;
    }

    const consults = consultFiles.map(f => `consult('${f}').`).join('');

    const prologCommand = `${consults} ${goal}`;

    return new Promise((resolve, reject) => {
      execFile("swipl", ["-q", "-g", prologCommand], (error, stdout, stderr) => {
        if (error) {
          reject(stderr || error.message);
        } else {
          resolve(stdout.trim() || "false");
        }
      });
    });
  };

  try {
    let consultFiles = [prologFile];

    if (userCode && userCode.trim()) {
      const tmpFilePath = path.join(os.tmpdir(), `user_code_${Date.now()}.pl`);
      fs.writeFileSync(tmpFilePath, userCode, "utf8");
      consultFiles.push(tmpFilePath);

      const result = await runProlog(consultFiles);
      fs.unlinkSync(tmpFilePath);
      return res.json({ result });
    } else {
      const result = await runProlog(consultFiles);
      return res.json({ result });
    }
  } catch (err) {
    return res.status(500).json({ error: String(err) });
  }
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
