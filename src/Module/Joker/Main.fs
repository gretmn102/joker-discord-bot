module Joker.Main
open FsharpMyExtension
open DSharpPlus
open Types
open Extensions
open Shared

open Joker

type State =
    {
        GuildSettings: Model.GuildSettings
    }

type Request =
    | SetIsEnabled of bool
    | GetIsEnabled
    | SetMessages of json: string
    | GetMessages

type Req =
    | Typing of DiscordClient * EventArgs.TypingStartEventArgs
    | InteractionRequest of EventArgs.InteractionCreateEventArgs * Request

type LocalState =
    {
        AuthorMember: Entities.DiscordMember option
        GuildId: GuildId option
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LocalState =
    let empty =
        {
            AuthorMember = None
            GuildId = None
        }

    let getGuildId getGuildId (localState: LocalState) =
        match localState.GuildId with
        | None ->
            let x = getGuildId ()
            let localState =
                { localState with
                    GuildId = Some x
                }
            x, localState

        | Some x ->
            x, localState

    let getAuthorMember getAuthorMember (localState: LocalState) =
        match localState.AuthorMember with
        | None ->
            let x = getAuthorMember ()
            let localState =
                { localState with
                    AuthorMember = Some x
                }
            x, localState

        | Some x ->
            x, localState

let reduce msg (state: State) =
    let interp getAuthorMember getGuildId response responseEphemeral (req: Model.Req) (state: State) =
        let rec loop (localState: LocalState) (req: Model.Req) (state: State) =
            match req with
            | Model.GuildSettingsDbReq cmd ->
                let guildId, localState = LocalState.getGuildId getGuildId localState
                let res, guildSettings = Model.GuildSettings.interp guildId cmd state.GuildSettings
                let state = {
                    state with
                        GuildSettings = guildSettings
                }
                loop localState res state

            | Model.GetAuthorNick((), next) ->
                let authorMember, localState = LocalState.getAuthorMember getAuthorMember localState
                let cmd = next authorMember.Nickname
                loop localState cmd state

            | Model.GetAuthorId((), next) ->
                let authorMember, localState = LocalState.getAuthorMember getAuthorMember localState
                let req = next authorMember.Id
                loop localState req state

            | Model.Response msg ->
                msg
                |> Option.iter (fun msg ->
                    let builder = Model.DiscordMessageSender.create msg
                    if msg.IsEphemeral then
                        responseEphemeral builder
                    else
                        response builder
                )

                state

        loop LocalState.empty req state

    match msg with
    | Typing(client, e) ->
        let response (b: Entities.DiscordMessageBuilder) =
            b.AddMentions(Entities.Mentions.All) |> ignore
            awaiti <| e.Channel.SendMessageAsync(b)

        interp
            (fun () -> getGuildMember e.Guild e.User)
            (fun () -> e.Guild.Id)
            response
            response
            Model.startJoke
            state

    | InteractionRequest(e, req) ->
        let response (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.AddMentions(Entities.Mentions.All) |> ignore
            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let responseEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- true

            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let interp action =
            interp
                (fun () -> getGuildMember e.Interaction.Guild e.Interaction.User)
                (fun () -> e.Interaction.Guild.Id)
                response
                responseEphemeral
                action
                state

        match req with
        | SetIsEnabled isEnabled ->
            Model.setIsEnabled isEnabled
        | GetIsEnabled ->
            Model.getIsEnabled
        | SetMessages(json) ->
            Model.setMessages json
        | GetMessages ->
            Model.getMessages
        |> interp

let createReducer db =
    let init = {
        GuildSettings = Model.GuildSettings.init "jokerGuildSettings" db
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! msg = mail.Receive()
                let state =
                    try
                        reduce msg state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let create (db: MongoDB.Driver.IMongoDatabase) =
    let reducer = createReducer db

    let commands =
        let prefs =
            let isEnabled =
                let commandName = "is-enabled"

                let setName = "set"
                let setOptName = "on-off"
                let setOptChoiceOn = "on"
                let setOptChoiceOff = "off"

                let getName = "get"

                {|
                    Command =
                        Entities.DiscordApplicationCommandOption(
                            commandName,
                            "turn on/off joker",
                            ApplicationCommandOptionType.SubCommandGroup,
                            options = [|
                                Entities.DiscordApplicationCommandOption(
                                    setName,
                                    "set",
                                    ApplicationCommandOptionType.SubCommand,
                                    options = [|
                                        Entities.DiscordApplicationCommandOption(
                                            setOptName,
                                            "on/off",
                                            ApplicationCommandOptionType.String,
                                            required = true,
                                            choices = [|
                                                Entities.DiscordApplicationCommandOptionChoice(setOptChoiceOn, setOptChoiceOn)
                                                Entities.DiscordApplicationCommandOptionChoice(setOptChoiceOff, setOptChoiceOff)
                                            |]
                                        )
                                    |]
                                )
                                Entities.DiscordApplicationCommandOption(
                                    getName,
                                    "get",
                                    ApplicationCommandOptionType.SubCommand
                                )
                            |]
                        )

                    Handler = fun (e: EventArgs.InteractionCreateEventArgs) (data: Entities.DiscordInteractionDataOption) ->
                        if data.Name = commandName then
                            data.Options
                            |> Seq.iter (fun data ->
                                if data.Name = getName then
                                    reducer.Post(InteractionRequest (e, GetIsEnabled))

                                elif data.Name = setName then
                                    let isEnabled =
                                        data.Options
                                        |> Seq.tryPick (fun x ->
                                            if x.Name = setOptName then
                                                let v = x.Value :?> string

                                                let isEnabled = v = setOptChoiceOn
                                                Some isEnabled
                                            else
                                                None
                                        )
                                    match isEnabled with
                                    | Some isEnabled ->
                                        reducer.Post(InteractionRequest (e, SetIsEnabled isEnabled))
                                    | None ->
                                        failwithf "not found `%A` in %A" setOptName (List.ofSeq data.Options)

                                else
                                    failwithf "%A not implemented yet" data.Name
                            )

                            true
                        else
                            false
                |}

            let messageTemplates =
                let commandName = "message-templates"

                let setName = "set"
                let setOptName = "json"

                let getName = "get"

                {|
                    Command =
                        Entities.DiscordApplicationCommandOption(
                            commandName,
                            "turn on/off joker",
                            ApplicationCommandOptionType.SubCommandGroup,
                            options = [|
                                Entities.DiscordApplicationCommandOption(
                                    setName,
                                    "set",
                                    ApplicationCommandOptionType.SubCommand,
                                    options = [|
                                        Entities.DiscordApplicationCommandOption(
                                            setOptName,
                                            "json",
                                            ApplicationCommandOptionType.String,
                                            required = true
                                        )
                                    |]
                                )
                                Entities.DiscordApplicationCommandOption(
                                    getName,
                                    "get",
                                    ApplicationCommandOptionType.SubCommand
                                )
                            |]
                        )

                    Handler = fun (e: EventArgs.InteractionCreateEventArgs) (data: Entities.DiscordInteractionDataOption) ->
                        if data.Name = commandName then
                            data.Options
                            |> Seq.iter (fun data ->
                                if data.Name = getName then
                                    reducer.Post(InteractionRequest (e, GetMessages))

                                elif data.Name = setName then
                                    let messages =
                                        data.Options
                                        |> Seq.tryPick (fun x ->
                                            if x.Name = setOptName then
                                                let v = x.Value :?> string
                                                Some v
                                            else
                                                None
                                        )
                                    match messages with
                                    | Some messages ->
                                        reducer.Post(InteractionRequest (e, SetMessages messages))
                                    | None ->
                                        failwithf "not found `%A` in %A" setOptName (List.ofSeq data.Options)

                                else
                                    failwithf "%A not implemented yet" data.Name
                            )

                            true
                        else
                            false
                |}

            let slashCommandName = "joker-prefs"

            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "Managing Joker settings",
                        [|
                            isEnabled.Command
                            messageTemplates.Command
                        |],
                        ``type`` = ApplicationCommandType.SlashCommand,
                        name_localizations = Map [
                            "ru", "joker-настройки"
                        ]
                    )

                Handler = fun e ->
                    e.Interaction.Data.Options
                    |> Seq.iter (fun data ->
                        let isHandled =
                            isEnabled.Handler e data
                            || messageTemplates.Handler e data

                        if not isHandled then
                            failwithf "%A not implemented yet" data.Name
                    )
            |}

        [|
            prefs
        |]

    { BotModule.empty with
        InteractionCommands =
            Some commands

        TypingStarted =
            let f (client, e: EventArgs.TypingStartEventArgs) =
                reducer.Post(Typing(client, e))

            Some f
    }
