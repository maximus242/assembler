FROM ubuntu:latest
COPY my_executable /app/my_executable
WORKDIR /app
RUN chmod +x my_executable
CMD ["./my_executable"]
