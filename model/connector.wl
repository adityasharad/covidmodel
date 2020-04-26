(* ::Package:: *)

SetDirectory[$HomeDirectory<>"/github/covid-modelling/cosmc"];
Import["model/model.wl"];

customScenario=<|"id"->"customScenario","distancingDays"->90,"distancingLevel"->0.5,"name"->"Custom", "gradual"->False|>;
(* We leave the existing scenarios so that param fitting can take place against them,
but add a new scenario that describes our input set of interventions.*)
scenarios=Append[scenarios, customScenario];
(* GenerateModelExport[1, {"CA"}]; *)
(* Print[scenarios[[8]]]; *)
(* Print[scenarios[[1]]]; *)
(* Print[stateDistancingPrecompute[[1]]]; *)

(* TODO: Set these simulation dates. Variables are globals from data.wl *)
(* tmax0 = 365 * 2;
tmin0 = 1;
may1=121; *)

(* Translates a date string into an integer,
which states the number of days from 1 Jan 2020 to the given date
(inclusive, starting at 1). *)
translateDateIntoOffset[dateString_]:=Module[{
  start2020,
  date,
  dateOffset
},
  start2020 = DateString["2020-01-01", "ISODate"];
  date=DateString[dateString, "ISODate"];
  dateOffset = DayCount[start2020, date] + 1;
  dateOffset
];

translateInput[inputPath_]:=Module[{
  requestInput,
  modelInput,
  stateCode,
  interventionPeriods,
  interventionStartDateOffsets,
  interventionEndDateOffsets,
  interventionDistancingLevels,
  interventionDistancing,
  fullDistancing
},
  (* RawJSON gives an association while JSON gives a list of rules. *)
  requestInput = Import[inputPath, "RawJSON"];
  modelInput = requestInput["configuration"];

  (* Only US states are currently supported *)
  (* If[modelInput["region"] != "US", "US", Throw["Only US states are currently supported."]]; *)
  (* Drop the US- prefix *)
  stateCode = StringDrop[modelInput["subregion"], 3];
  interventionPeriods = modelInput["parameters"]["interventionPeriods"];
  (* TODO: Translate the list of interventions *)
  (* Here we use the estimated reduction in population contact from the input.
  This is in [0..100] (0 = no distancing, 100 = total isolation).
  Turn it into a distancing level in [0..1] (0 = total isolation, 1 = no distancing).
  TODO: Should we use the named interventions and their intensity? *)
  interventionDistancingLevels = Map[((100-#["reductionPopulationContact"])/100.)&, interventionPeriods];
  interventionStartDateOffsets = Map[translateDateIntoOffset[#["startDate"]]&, interventionPeriods];
  (* Treat start dates as inclusive and end dates as exclusive.
  endDate[i] = startDate[i+1] for 1 <= i < len, and endDate[len] = tMax+1 *)
  interventionEndDateOffsets = Drop[Append[interventionStartDateOffsets, tmax0+1], 1];

  (* Policy distancing as a time series, describing the distancing level at each day.
  0 = 100% contact reduction/total isolation.
  1 = 0% contact reduction/no distancing.
  Duration of each intervention: endDate[i]-startDate[i].
  Note this treats each period as being start-inclusive, end-exclusive.
  *)
  interventionDistancing = MapThread[
    Function[
      {startOffset, endOffset, distancingLevel},
      ConstantArray[distancingLevel, endOffset-startOffset]
    ],
    {
      interventionStartDateOffsets,
      interventionEndDateOffsets,
      interventionDistancingLevels
    }
  ];

  fullDistancing = Flatten[{
    (* Pre-policy distancing - constant at 1 from 1 Jan 2020 to start of policy.*)
    ConstantArray[1., interventionStartDateOffsets[[1]]-1],
    (* TODO: Should we use historical distancing data?
    Here we assume it is included in the inputs from the UI.*)
    interventionDistancing
    (* Post-policy distancing is assumed to be included as the last intervention period.*)
  }];
  {fullDistancing, Length[fullDistancing], stateCode}
]
Print[translateInput["model/data/inputFile.json"]];