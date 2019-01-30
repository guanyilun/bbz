module bbz

@enum Acts begin
    Bor
    Def
    Zan
end

@enum AgentType begin
    RandAI
    CLI
end

struct Player
    qi::UInt8
    agentType::AgentType
end # struct

PlayerIDs = Set([:P1, :P2])

struct LiveState
    P1::Player
    P2::Player
end # struct

struct EndState
    winner::Symbol
end # struct

State = Union{LiveState, EndState}

function react(state::State, agentType::AgentType)::Acts
    agentType == RandAI ? rand(collect(instances(Acts))) :
        agentType == CLI ? cli_react(state::State) :
            error("Unknown Agent Type!")
end # function

function cli_react(state::State)::Acts
    write(stdout, "\nCurrent state: \n")
    write(stdout, repr(state) * "\n")
    write(stdout, "Your action is: \n")
    keystroke = readline(stdin)
    if keystroke == "b"
        Bor
    elseif keystroke == "z"
        Zan
    elseif keystroke == "d"
        Def
    else
        write(stdout, "\nOpps...")
        write(stdout, repr(typeof(keystroke)))
        write(stdout, repr(keystroke))
        cli_react(state)
    end
end #function

function zan(player::Player)::Player
    Player(player.qi + UInt8(1), player.agentType)
end # function

function bor(player::Player)::Player
    Player(UInt8(0), player.agentType)
end # function

function step(state::LiveState)::State
    write(stdout, "\n******* \n")
    playerActions = Dict{Symbol, Acts}()
    for playerID in PlayerIDs
        player = getfield(state, playerID)
        playerActions[playerID] = react(state, player.agentType)
        write(stdout, "\n")
        write(stdout, repr(playerID))
        write(stdout, " played ")
        write(stdout, repr(playerActions[playerID]))
    end # for
    write(stdout, "\n")
    if playerActions[:P1] == Zan && playerActions[:P2] == Zan
        player = state.P1
        LiveState(zan(state.P1),
                  zan(state.P2))
    elseif playerActions[:P1] == Bor && playerActions[:P2] == Zan
        if state.P1.qi > 0x00
            EndState(:P1)
        else
            LiveState(state.P1,
                      zan(state.P2))
        end # if
    elseif playerActions[:P2] == Bor && playerActions[:P1] == Zan
        if state.P2.qi > 0x00
            EndState(:P2)
        else
            LiveState(zan(state.P1),
                      state.P2)
        end # if
    elseif playerActions[:P1] == Zan && playerActions[:P2] == Def
        LiveState(zan(state.P1),
                  state.P2)
    elseif playerActions[:P2] == Zan && playerActions[:P1] == Def
        LiveState(state.P1,
                  zan(state.P2))
    elseif playerActions[:P2] == Bor && playerActions[:P1] == Def
        if state.P2.qi > 0x01
            EndState(:P2)
        else
            LiveState(state.P1,
                      bor(state.P2))
        end # if
    elseif playerActions[:P1] == Bor && playerActions[:P2] == Def
        if state.P1.qi > 0x01
            EndState(:P1)
        else
            LiveState(bor(state.P1),
                      state.P2)
        end # if
    elseif playerActions[:P1] == Bor && playerActions[:P2] == Bor
        if state.P1.qi > state.P2.qi
            EndState(:P1)
        elseif state.P1.qi < state.P2.qi
            EndState(:P2)
        else
            LiveState(bor(state.P1),
                      bor(state.P2))
        end # if
    elseif playerActions[:P1] == Def && playerActions[:P2] == Def
        state
    end # if

end # function

function step(state::EndState)
    write(stdout, "\n******* \n")
    write(stdout, "Game over with winner")
    write(stdout, repr(state.winner))
    exit(0)
end # function

function run(state::LiveState)::State
    for i = 1:100
        state = step(state)
    end # for
    state
end # function

init = LiveState(Player(UInt8(0), CLI),
                 Player(UInt8(0), RandAI))
run(init)

end # module
