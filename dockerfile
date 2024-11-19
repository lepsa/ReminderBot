# Get all the deps built for both the exe and tests
FROM haskell:9.6-bullseye AS build

WORKDIR /opt/reminderbot

# Build the code
RUN cabal update
COPY ./ReminderBot.cabal /opt/reminderbot/ReminderBot.cabal
RUN cabal build --only-dependencies -j$(nproc)

# Copy the required files into the docker image
COPY ./app /opt/reminderbot/app
COPY ./src /opt/reminderbot/src
COPY ./test /opt/reminderbot/test
COPY ./CHANGELOG.md /opt/reminderbot/CHANGELOG.md
COPY ./LICENSE /opt/reminderbot/LICENSE

# Install the bot into the working directory
RUN cabal install --installdir=. --install-method=copy exe:ReminderBot
RUN strip ReminderBot

# What actually runs, no haskell compiler stuff
FROM debian:bullseye AS reminderbot

# Copy everything we need from the build image into the running image
COPY --from=build /opt/reminderbot/ReminderBot /opt/reminderbot/ReminderBot

# Update the base system
RUN apt-get update
RUN apt-get install -y ca-certificates

WORKDIR /opt/reminderbot

# Run the bot
CMD /opt/reminderbot/ReminderBot /opt/reminderbot/ReminderBotConfig/config.json
