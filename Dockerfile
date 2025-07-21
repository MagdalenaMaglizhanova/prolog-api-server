# Използваме официален Node.js базов образ
FROM node:20-slim

# Инсталираме SWI-Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Създаваме директория за приложението
WORKDIR /app

# Копираме package.json и package-lock.json (ако имаш)
COPY package*.json ./

# Инсталираме зависимостите
RUN npm install

# Копираме останалия код
COPY . .

# Дефинираме порта (Render ще го открие автоматично)
ENV PORT=10000

# Стартираме сървъра
CMD ["node", "server.js"]
