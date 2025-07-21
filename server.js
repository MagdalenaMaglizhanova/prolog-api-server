const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const os = require("os");

const app = express();
const port = process.env.PORT || 3000;

app.use(cors());
app.use(express.json());

// Allowed prolog files for security
const allowedFiles = ["example1.pl", "mineral_water.pl", "history.pl"];

app.post("/prolog", async (req, res) => {
  const { query, file, userCode } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!file) return res.status(400).json({ error: "No file specified" });
  if (!allowedFiles.includes(file)) return res.status(400).json({ error: "File not allowed" });

  // Base Prolog file path
  const prologFile = path.join(__dirname, "prolog_files", file);

  // Function to run the prolog process
  const runProlog = (consultFiles) => {
    // Determine if query contains variables (uppercase letters)
    const hasVars = /[A-Z]/.test(query);

    // Construct the Prolog goal
    let goal = "";
    if (hasVars) {
      // Extract args inside parentheses
      const argsMatch = query.match(/\((.*)\)/);
      const args = argsMatch ? argsMatch[1] : "";
      goal = `findall([${args}], (${query}), L), writeq(L), nl, halt.`;
    } else {
      goal = `${query}, write('true'), nl, halt.`;
    }

    // Build consult command string for all files
    const consults = consultFiles.map(f => `consult('${f}').`).join('');

    // Full Prolog command: consult files then run goal
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
      // Create temp file with user code
      const tmpFilePath = path.join(os.tmpdir(), `user_code_${Date.now()}.pl`);
      fs.writeFileSync(tmpFilePath, userCode, "utf8");
      consultFiles.push(tmpFilePath);

      // Run prolog with both files, then delete temp after
      const result = await runProlog(consultFiles);
      fs.unlinkSync(tmpFilePath);
      return res.json({ result });
    } else {
      // Run prolog with just the selected file
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
