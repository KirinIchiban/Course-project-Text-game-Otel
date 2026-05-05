module HotelGame.Types

type GameState = {
    Location: string
    Inventory: string list
    Doors: (string * string) list
    Items: Map<string, string>
    Characters: Map<string, string>
    Clues: Map<string, string>
    TalkedTo: Map<string, int>
    InvestigationStarted: bool
    LugerAvailable: bool
    HinkleFound: bool
    SafeOpen: bool
    DialogCount: int
    LockedDoors: (string * string) list
    CluesFound: string list
    EventsTriggered: string list
    RequiredTalks: string list
    OlafBodyExamined: bool      
    HinkusBagExamined: bool     
    RoomEnteredCount: Map<string, int> 
    SnowDummyDiscoveryCount: int
    BrunnInterrogated: bool     
    HinkusConfessed: bool       
    MosesInterrogated: bool      
    AllCluesCollected: bool   
    CaseInSafe: bool   
    OlafSuitcaseExamined : bool
    OlafSuitcaseMoved : bool
    NoteFound : bool
    NoteRead : bool
    MuseumNoiseTriggered : bool
}
