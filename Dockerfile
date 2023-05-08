FROM swipl:9.0.4
COPY ./back /app
CMD ["swipl", "/app/server.pl"]