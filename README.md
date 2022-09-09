# learning-bot

Here is a telegram bot learning project. Bot receives messages from Telegram users and replies to them with the same message.
Using commands in chat with bot, user can choose the number of repetitions.

<h3> Available commands:</h3>

  `/help` — to get a list of commands;
  
  `/repeat` — to select the number of repetitions;

Also, there is console version of this bot. The bot version is selected by filling the config.

In addition, config allows user to select logging level, path to the log file, text of service messages for users, and default value for the number of repetitions.

<h1> Deployment </h1> 

1. Clone the repository;
2. Fill in the `botConfigTemplate.json` file and rename it to `botConfig.json`. All fields except tgToken and frontEndType are filled with default values. In `tgToken`, instead of an ellipsis, you need to enter your bot's token. In `frontEndType`, you need to specify either “console” to launch a console bot, or “telegram” to launch a telegram bot;
3. The bot is launched by the `stack run` command from the terminal opened in the project folder;

<h1>  Architecture </h1> 

The project uses **Handle-Pattern**. Common logic for telegram bot and console bot turned out to be the message processing logic. It has been moved to `App.MessageHandling` module. There are **unit-tests** for this logic.

There are separate modules for the implementation of each bot and for filling the handler: `App.ConsoleBotRun` and `App.TgBotRun`.
Modules for reading the config and logging are located in the `Implementations` folder.
