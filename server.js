// server.js
const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10000;

app.use(cors());
app.use(express.json());

// Supabase client
const supabaseUrl = process.env.SUPABASE_URL;
const supabaseKey = process.env.SUPABASE_KEY;
const supabase = createClient(supabaseUrl, supabaseKey);

// Helper: fetch file from Supabase
async function fetchSupabaseFile(filename) {
    const { data, error } = await supabase
        .from("prologFiles")
        .select("content")
        .eq("name", filename)
        .single();
    if (error) throw error;
    return data.content;
}

// POST /prolog-run
app.post("/prolog-run", async (req, res) => {
    let { code, query } = req.body;

    try {
        if (!code && !query) {
            return res.status(400).json({ error: "No code or query provided" });
        }

        // Проверка за consult('file.pl')
        const consultMatch = query?.match(/consult\(['"](.+\.pl)['"]\)/);
        if (consultMatch) {
            const filename = consultMatch[1];
            // Зареждаме файла от Supabase
            code = await fetchSupabaseFile(filename);
            if (!code) {
                return res.status(404).json({ error: `File ${filename} not found` });
            }
            query = ""; // няма конкретна заявка, само консултация
        }

        // Създаваме временен Prolog файл
        const tmpFile = path.join(__dirname, "temp.pl");
        fs.writeFileSync(tmpFile, code);

        // Подготвяме целта
        let goal = "";
        if (query) {
            const hasVars = /[A-Z]/.test(query);
            if (hasVars) {
                const variableRegex = /[A-Z][a-zA-Z0-9_]*/g;
                const variables = query.match(variableRegex);
                const args = variables ? variables.join(",") : "";
                goal = `findall([${args}], ${query}, L), writeq(L), nl, halt.`;
            } else {
                goal = `${query}, write('true'), nl, halt.`;
            }
        } else {
            goal = "write('File consulted successfully'), nl, halt.";
        }

        // Изпълняваме SWI-Prolog
        execFile("swipl", ["-q", "-s", tmpFile, "-g", goal], (error, stdout, stderr) => {
            if (error) {
                console.error("Prolog Error:", error);
                console.error("Prolog Stderr:", stderr);
                return res.status(500).json({ error: stderr || error.message });
            }
            res.json({ result: stdout.trim() || "false" });
        });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: err.message });
    }
});

app.listen(port, () => {
    console.log(`Prolog compiler server running on port ${port}`);
});
