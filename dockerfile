# Get all the deps built for both the exe and tests
FROM haskell:9 as build

WORKDIR /opt/reminderbot

RUN cabal update
COPY ./ReminderBot.cabal /opt/reminderbot/ReminderBot.cabal
RUN cabal build --only-dependencies -j$(nproc)

COPY ./app /opt/reminderbot/app
COPY ./src /opt/reminderbot/src
COPY ./test /opt/reminderbot/test
COPY ./CHANGELOG.md /opt/reminderbot/CHANGELOG.md
COPY ./LICENSE /opt/reminderbot/LICENSE

RUN cabal install --installdir=. --install-method=copy exe:ReminderBot

# What actually runs, no haskell compiler stuff
FROM debian:buster as reminderbot

COPY --from=build /opt/reminderbot/ReminderBot /opt/reminderbot/ReminderBot

WORKDIR /opt/reminderbot

CMD ["/opt/reminderbot/ReminderBot"]