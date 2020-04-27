(* ::Package:: *)

SetDirectory[$HomeDirectory<>"/github/covid-modelling/cosmc"];
Import["model/data.wl"];

(* TODO: Set these simulation dates. Variables are globals from data.wl *)
(* tmax0 = 365 * 2;
tmin0 = 1;
may1=121; *)

(* Translates a date string into an integer,
which states the number of days from 1 Jan 2020 to the given date
(inclusive, starting at 0). *)
translateDateIntoOffset[dateString_]:=Module[{
  start2020,
  date,
  dateOffset
},
  start2020 = DateString["2020-01-01", "ISODate"];
  date=DateString[dateString, "ISODate"];
  dateOffset = DayCount[start2020, date];
  dateOffset
];

translateInput[inputPath_]:=Module[{
  modelInput,
  stateCode,
  interventionPeriods,
  interventionStartDateOffsets,
  interventionEndDateOffsets,
  interventionDistancingLevels,
  interventionDistancing,
  fullDistancing,
  smoothedFullDistancing,
  SlowJoin,
  smoothing,
  distancingFunction,
  fullDays
},
  (* Read the JSON input to the model.
  Use RawJSON to obtain an association. (JSON gives a list of rules.) *)
  modelInput = Import[inputPath, "RawJSON"]["configuration"];

  (* Only US states are currently supported *)
  (* If[modelInput["region"] != "US", "US", Throw["Only US states are currently supported."]]; *)
  (* Drop the US- prefix *)
  stateCode = StringDrop[modelInput["subregion"], 3];
  
  interventionPeriods = modelInput["parameters"]["interventionPeriods"];
  (* Here we use the estimated reduction in population contact from the input.
  This is in [0..100] (0 = no distancing, 100 = total isolation).
  Turn it into a distancing level in [0..1] (0 = total isolation, 1 = no distancing).
  TODO: Should we use the named interventions and their intensity? *)
  interventionDistancingLevels = Map[((100-#["reductionPopulationContact"])/100.)&, interventionPeriods];
  interventionStartDateOffsets = Map[translateDateIntoOffset[#["startDate"]]&, interventionPeriods];
  (* Treat start dates as inclusive and end dates as exclusive.
  endDate[i] = startDate[i+1] for 1 <= i < len, and endDate[len] = tMax+1
  This assumes post-policy distancing is provided as the last intervention period. *)
  interventionEndDateOffsets = Drop[Append[interventionStartDateOffsets, tmax0+1], 1];

  (* List of lists describing policy distancing from interventions.
  Each list is a time series for one intervention period,
  with the distancing level at each day.
  0 = 100% contact reduction/total isolation.
  1 = 0% contact reduction/no distancing.
  *)
  interventionDistancing = Prepend[
    MapThread[
      Function[
        {startOffset, endOffset, distancingLevel},
        (* Duration of each intervention: endDate[i]-startDate[i].
        Note this treats each period as being start-inclusive, end-exclusive. *)
        ConstantArray[distancingLevel, endOffset-startOffset]
      ],
      {
        interventionStartDateOffsets,
        interventionEndDateOffsets,
        interventionDistancingLevels
      }
    ],
    (* Pre-policy distancing - constant at 1 from 1 Jan 2020 to start of policy.*)
    ConstantArray[1., interventionStartDateOffsets[[1]]]
    (* TODO: Should we use historical distancing data?
    Here we assume it is already included in the inputs from the UI.*)
  ];

  (* Flatten the list of lists into a single time series list. *)
  fullDistancing = Flatten[interventionDistancing];

  (* TODO: These are copied from modules in data.wl,
  and should be shared instead. *)
  smoothing = 3;
  SlowJoin := Fold[Module[{smoother},
      smoother=1-Exp[-Range[Length[#2]]/smoothing];
      Join[#1, Last[#1](1-smoother)+#2 smoother]]&];
  fullDays = Range[0, tmax0];
  smoothedFullDistancing = SlowJoin[interventionDistancing];

  (* Domain and range length must match for us to interpolate. *)
  On[Assert];
  Assert[Length[fullDistancing] == Length[fullDays]];
  Off[Assert];

  distancingFunction = Interpolation[
    Transpose[{
      fullDays,
      smoothedFullDistancing
    }],
    InterpolationOrder->3
  ];

  {
    <|
      "distancingDays"->fullDays,
      (* Deliberately omitted: distancingLevel. TODO: Check if used, or relevant for multiple interventions. *)
      "distancingData"->fullDistancing,
      "distancingFunction"->distancingFunction
      (* Deliberately omitted: mostRecentDistancingDay *)
    |>,
    stateCode
  }
]
{customDistancing, stateCode} = translateInput["model/data/inputFile.json"];
Print["Length of distancingDays: ", Length[customDistancing["distancingDays"]]];
Print["Length of distancingData: ", Length[customDistancing["distancingData"]]];
Print["Will run model for state " <> stateCode];
customScenario=<|"id"->"customScenario","name"->"Custom", "gradual"->False|>;

(* We leave the existing scenarios so that param fitting can take place against them,
but add a new scenario and distancing function that describes our input set of interventions.
These are defined in the `data` package but used in the model initialisation.
So we modify them here, between the two imports.
*)
(* scenarios=Append[scenarios, customScenario]; *)
Print["Adding a custom scenario and distancing function to the precomputed data"];
scenarios={scenario1, customScenario};
Print["Scenarios rewritten: ", scenarios];
stateDistancingPrecompute[stateCode] = Append[
  stateDistancingPrecompute[stateCode],
  customScenario["id"] -> customDistancing
];


(* Import the `model` package, but ensure it does not re-import the `data` package,
since we have already imported from `data` and modified its global variables. *)
isDataImported = True
Import["model/model.wl"];

CreateDirectory["public/json/"<>stateCode<>"/"<>customScenario["id"]];
Print["Scenarios after import of model.wl: ", scenarios];
Print["Checking additional distancing data"];
Print["Precomputed distancing keys: ", Keys[stateDistancingPrecompute[stateCode]]];
Print["Precomputed distancing days: ", stateDistancingPrecompute[stateCode][customScenario["id"]]["distancingDays"]];
Print["Precomputed distancing data: ", stateDistancingPrecompute[stateCode][customScenario["id"]]["distancingData"]];

Print["Running model"];
GenerateModelExport[1, {stateCode}];