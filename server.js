const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");

const app = express();
const port = process.env.PORT || 10000;

app.use(cors());
app.use(express.json());

app.post("/prolog", (req, res) => {
  const { query } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });

  // Винаги стартираме от manager.pl
  const prologFile = path.join(__dirname, "prolog_files", "manager.pl");

  // Целта е просто да изпълним това, което е подадено
  const goal = `${query}, halt.`;

  execFile(
    "swipl",
    ["-q", "-s", prologFile, "-g", goal],
    (error, stdout, stderr) => {
      if (error) {
        res.status(500).json({ error: stderr || error.message });
      } else {
        res.json({ result: stdout.trim() || "false" });
      }
    }
  );
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
