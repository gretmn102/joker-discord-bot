module Joker.Model
open FsharpMyExtension
open FsharpMyExtension.Either
open MongoDB.Driver
open MongoDB.Bson
open Db

open Types
open Shared

/// TODO: move to view?
module DiscordMessageSender =
    open DSharpPlus

    type Embed =
        {
            Description: string
        }

    type Message =
        {
            Content: string option
            Embed: Embed option
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Message =
        let createSimple content =
            {
                Content = Some content
                Embed = None
            }

    type Builder =
        {
            IsEphemeral: bool
            Message: Message
        }

    let create (builder: Builder) =
        let b = Entities.DiscordMessageBuilder()
        let message = builder.Message

        message.Content
        |> Option.iter (fun content ->
            b.Content <- content
        )

        b

type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

type GuildSettingData =
    {
        IsEnabled: bool
        Messages: string []
    }
    static member Init isEnabled messages =
        {
            IsEnabled = isEnabled
            Messages = messages
        }
    static member Empty =
        {
            IsEnabled = false
            Messages = [||]
        }
    static member Serialize (data: GuildSettingData) =
        data |> Json.ser
    static member Deserialize json =
        try
            Ok (Json.des json)
        with e ->
            Error e.Message

type GuildSettingVersion =
    | V0 = 0

type GuildSettingId = GuildId

type GuildSetting = CommonDb.Data<GuildSettingId, GuildSettingVersion, GuildSettingData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildSetting =
    let create id data: GuildSetting =
        CommonDb.Data.create id GuildSettingVersion.V0 data

type GuildSettingsDbReq<'Next> =
    | Get of Req<unit, GuildSetting option, 'Next>
    | Set of Req<GuildSetting, unit, 'Next>
    | GetEmpty of Req<unit, GuildSetting, 'Next>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildSettingReq =
    let get args next =
        Get(args, next)

    let set args next =
        Set(args, next)

    let empty args next =
        GetEmpty(args, next)

type Req =
    | GuildSettingsDbReq of GuildSettingsDbReq<Req>
    | GetAuthorId of Req<unit, UserId, Req>
    | GetAuthorNick of Req<unit, string, Req>
    | Response of DiscordMessageSender.Builder option
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Req =
    let guildSettingReq fn arg next =
        GuildSettingsDbReq (fn arg (fun res ->
            next res
        ))

    let getAuthorId arg next =
        GetAuthorId(arg, next)

    let getAuthorNick arg next =
        GetAuthorNick(arg, next)

    let responseNone =
        Response None

    let response msg =
        Response (Some msg)

let r = System.Random ()

let startJoke =
    let getGuildSetting () next =
        pipeBackwardBuilder {
            let! guildSetting = Req.guildSettingReq GuildSettingReq.get ()
            match guildSetting with
            | Some x -> return next x
            | None -> return Req.responseNone
        }

    let testIsEnabled (guildSetting: GuildSetting) next =
        if guildSetting.Data.IsEnabled then
            next ()
        else
            Req.responseNone

    let testIsNotEmpty messages next =
        if Array.isEmpty messages then
            Req.responseNone
        else
            next ()

    let parseMessageTemplate rawMessage next =
        match MessageTemplate.Message.parse rawMessage with
        | Right message ->
            next message
        | Left _ ->
            Req.responseNone

    pipeBackwardBuilder {
        let! guildSetting = getGuildSetting ()
        do! testIsEnabled guildSetting
        let rawMessages = guildSetting.Data.Messages
        do! testIsNotEmpty rawMessages
        let rawMessage = rawMessages.[r.Next(0, rawMessages.Length)]
        let! message = parseMessageTemplate rawMessage
        let! authorId = Req.getAuthorId ()
        let authorMention = sprintf "<@%d>" authorId
        let! authorNick = Req.getAuthorNick ()
        let content = MessageTemplate.Message.substitute authorMention authorNick message
        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = false
                Message = DiscordMessageSender.Message.createSimple content
            }
        return Req.response message
    }

let getGuildSetting () next =
    pipeBackwardBuilder {
        let! guildSetting = Req.guildSettingReq GuildSettingReq.get ()
        match guildSetting with
        | Some x -> return next x
        | None ->
            let! empty = Req.guildSettingReq GuildSettingReq.empty ()
            return next empty
    }

let setIsEnabled isEnabled =
    pipeBackwardBuilder {
        let! guildSetting = getGuildSetting ()
        let guildSetting =
            {
                guildSetting with
                    Data =
                        {
                            guildSetting.Data with
                                IsEnabled = isEnabled
                        }
            }
        do! Req.guildSettingReq GuildSettingReq.set guildSetting
        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = true
                Message = DiscordMessageSender.Message.createSimple "Done!"
            }
        return Req.response message
    }

let getIsEnabled =
    pipeBackwardBuilder {
        let! guildSetting = getGuildSetting ()
        let res =
            if guildSetting.Data.IsEnabled then
                "enabled"
            else
                "disabled"
        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = true
                Message =
                    sprintf "Joker is %s" res
                    |> DiscordMessageSender.Message.createSimple
            }
        return Req.response message
    }

module Json =
    let tryDes json =
        try
            Ok (Json.des json)
        with e ->
            Error e.Message

let setMessages messagesJson =
    let createSomethingWrongMessage errMsg =
        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = true
                Message =
                    sprintf "something wrong:\n```\n%s\n```" errMsg
                    |> DiscordMessageSender.Message.createSimple
            }
        message

    let parseMessages json next =
        pipeBackwardBuilder {
            let (res: Result<string [], _>) = Json.tryDes json
            match res with
            | Ok message ->
                return next message
            | Error errMsg ->
                return Req.response (createSomethingWrongMessage errMsg)
        }

    let testMessage rawMessage next =
        pipeBackwardBuilder {
            match MessageTemplate.Message.parse rawMessage with
            | Right message ->
                return next message
            | Left errMsg ->
                return Req.response (createSomethingWrongMessage errMsg)
        }

    let testMessages rawMessages next =
        let rec loop = function
            | x::xs ->
                testMessage x (fun x ->
                    loop xs
                )
            | [] ->
                next ()

        loop (List.ofArray rawMessages)

    let setMessages rawMessages next =
        pipeBackwardBuilder {
            let! guildSetting = getGuildSetting ()
            let guildSetting =
                {
                    guildSetting with
                        Data =
                            { guildSetting.Data with
                                Messages = rawMessages
                            }
                }
            do! Req.guildSettingReq GuildSettingReq.set guildSetting
            return next ()
        }

    pipeBackwardBuilder {
        let! rawMessages = parseMessages messagesJson
        do! testMessages rawMessages

        do! setMessages rawMessages

        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = true
                Message =
                    "Done!"
                    |> DiscordMessageSender.Message.createSimple
            }
        return Req.response message
    }

let getMessages =
    pipeBackwardBuilder {
        let! guildSetting = getGuildSetting ()

        let messages =
            guildSetting.Data.Messages
            |> Array.map (sprintf "* %s")
            |> String.concat "\n"

        let message: DiscordMessageSender.Builder =
            {
                IsEphemeral = true
                Message =
                    sprintf "List:\n%s" messages
                    |> DiscordMessageSender.Message.createSimple
            }
        return Req.response message
    }

type GuildSettings = CommonDb.GuildData<GuildSettingId, GuildSettingVersion, GuildSettingData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildSettings =
    let createData id =
        GuildSetting.create id GuildSettingData.Empty

    let init collectionName (db: IMongoDatabase): GuildSettings =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    match ver with
                    | GuildSettingVersion.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<GuildSetting>(doc)
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    failwithf "Not found version for:\n%A" doc
            )
            collectionName
            db

    let set userId setAdditionParams (guildData: GuildSettings) =
        CommonDb.GuildData.set
            createData
            userId
            setAdditionParams
            guildData

    let drop (db: IMongoDatabase) (items: GuildSettings) =
        CommonDb.GuildData.drop db items

    let tryFindById id (items: GuildSettings): GuildSetting option =
        CommonDb.GuildData.tryFind id items

    let rec interp guildId (req: GuildSettingsDbReq<_>) (guildSettings: GuildSettings) =
        match req with
        | Get((), next) ->
            let guildSetting = tryFindById guildId guildSettings
            next guildSetting, guildSettings
        | Set(arg, next) ->
            let guildSettings = set arg.Id (fun _ -> arg.Data) guildSettings
            next (), guildSettings
        | GetEmpty((), next) ->
            let guildSetting = GuildSetting.create guildId GuildSettingData.Empty
            next guildSetting, guildSettings
