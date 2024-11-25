FROM golang:latest
WORKDIR /app
COPY ./backend/go.mod ./backend/go.sum ./
RUN go mod download
RUN go build -o app .
COPY ./frontend/package.json ./frontend/package.json
RUN apt update
RUN apt install nodejs npm -y
WORKDIR /app/frontend
RUN npm install
COPY ./frontend .
RUN npm run build
WORKDIR /app
COPY ./backend .
RUN mv ./frontend/build .
EXPOSE 4000
CMD ["./app"]
