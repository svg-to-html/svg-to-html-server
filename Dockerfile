FROM openjdk:8-alpine

COPY target/uberjar/svg-to-html.jar /svg-to-html/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/svg-to-html/app.jar"]
