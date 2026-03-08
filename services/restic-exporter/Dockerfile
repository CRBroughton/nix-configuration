FROM node:22-alpine AS builder

WORKDIR /app

COPY package.json ./
RUN npm install

COPY tsconfig.json main.ts ./
RUN npm run build

FROM node:22-alpine

# Install restic
RUN apk add --no-cache restic

WORKDIR /app

COPY package.json ./
RUN npm install --omit=dev

COPY --from=builder /app/dist ./dist

ENV PORT=8080
EXPOSE 8080

CMD ["node", "dist/main.js"]
