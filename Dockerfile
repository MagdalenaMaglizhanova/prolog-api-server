# Използваме официалния Node образ
FROM node:18

# Инсталираме Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    apt-get clean

# Създаваме работна директория
WORKDIR /app

# Копираме файловете
COPY . .

# Инсталираме Node зависимости
RUN npm install

# Стартираме приложението
CMD ["node", "index.js"]
