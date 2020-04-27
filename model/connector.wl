(* ::Package:: *)

(* Connector that translates between the model and the input/output schema
used by the GitHub unified modelling UI. *)

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
  date
},
  start2020 = DateString["2020-01-01", "ISODate"];
  date=DateString[dateString, "ISODate"];
  DayCount[start2020, date]
];

translateInput[inputPath_]:=Module[{
  modelInput,
  stateCode,
  interventionPeriods,
  interventionDistancingLevels,
  interventionStartDateOffsets,
  interventionEndDateOffsets,
  interventionDistancing,
  fullDistancing,
  smoothing,
  SlowJoin,
  fullDays
  smoothedFullDistancing,
  distancingFunction
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
      (* Deliberately omitted: distancingLevel. *)
      "distancingData"->fullDistancing,
      "distancingFunction"->distancingFunction
      (* Deliberately omitted: mostRecentDistancingDay. *)
    |>,
    stateCode
  }
]
Print["Translating input from unified UI"];
{customDistancing, stateCode} = translateInput["model/data/inputFile.json"];
Print["Length of distancingDays: ", Length[customDistancing["distancingDays"]]];
Print["Length of distancingData: ", Length[customDistancing["distancingData"]]];
Print["Will run model for state " <> stateCode];

(* We leave the existing scenarios so that param fitting can take place against them,
but add a new scenario and distancing function that describes our input set of interventions.
These are defined in the `data` package but used in `model`.
So we modify them here, between the two imports.
*)
customScenario=<|"id"->"customScenario","name"->"Custom", "gradual"->False|>;
Print["Adding a custom scenario and distancing function to the precomputed data"];
(* For simplicity, remove all other scenarios,
except scenario1 which is needed for fitting.
scenarios=Append[scenarios, customScenario]; *)
scenarios={scenario1, customScenario};
stateDistancingPrecompute[stateCode] = Append[
  stateDistancingPrecompute[stateCode],
  customScenario["id"] -> customDistancing
];

(* Import the `model` package, but ensure it does not re-import the `data` package,
since we have already imported from `data` and modified its global variables. *)
isDataImported = True
Import["model/model.wl"];

CreateDirectory["public/json/"<>stateCode<>"/"<>customScenario["id"]];
Print["Modified scenarios: ", scenarios];
Print["Precomputed distancing keys: ", Keys[stateDistancingPrecompute[stateCode]]];
Print["Precomputed distancing days for custom scenario: ", stateDistancingPrecompute[stateCode][customScenario["id"]]["distancingDays"]];
Print["Precomputed distancing data for custom scenario: ", stateDistancingPrecompute[stateCode][customScenario["id"]]["distancingData"]];

Print["Running model"];
GenerateModelExport[10, {stateCode}];