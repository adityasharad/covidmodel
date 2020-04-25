(* ::Package:: *)

SetDirectory[$HomeDirectory<>"/github/covid-modelling/cosmc"];
Import["model/model.wl"];

customScenario=<|"id"->"customScenario","distancingDays"->90,"distancingLevel"->0.5,"name"->"Custom", "gradual"->False|>;
(* We leave the existing scenarios so that param fitting can take place against them,
but add a new scenario that describes our input set of interventions.*)
scenarios=Append[scenarios, customScenario];
(* Print[scenarios]; *)
(* GenerateModelExport[1, {"CA"}]; *)
(* Print[scenarios[[8]]]; *)
(* Print[scenarios[[1]]]; *)
(* Print[stateDistancingPrecompute[[1]]]; *)

(* TODO: Set these simulation dates. Variables are globals from data.wl *)
(* tmax0 = 365 * 2;
tmin0 = 1;
may1=121; *)

translateInput[inputPath_]:=Module[{
  requestInput,
  modelInput,
  stateCode,
  interventions,
  translateDateIntoOffset,
  interventionStartDateOffsets,
  interventionEndDateOffsets
},
  (* RawJSON gives an association while JSON gives a list of rules. *)
  requestInput = Import[inputPath, "RawJSON"];
  modelInput = requestInput["configuration"];

  (* Only US states are currently supported *)
  (* If[modelInput["region"] != "US", "US", Throw["Only US states are currently supported."]]; *)
  (* Drop the US- prefix *)
  stateCode = StringDrop[modelInput["subregion"], 3];
  (* TODO Translate the list of interventions *)
  interventions = modelInput["parameters"]["interventions"];

  (* Translates a date string into an integer,
  which states the number of days from 1 Jan 2020 to the given date
  (inclusive, starting at 1). *)
  translateDateIntoOffset[dateString_]:=Module[{
    start2020,
    date,
    dateOffset
  }
    start2020 = DateList[{2020, 1, 1, 0, 0, 0}];
    date=DateList[{dateString, {"Year", "Month", "Day"}}]
    (* TODO verify *)
    dateOffset = DayCount[start2020, startDate, "Days"] + 1;
    dateOffset
  ]

  interventionStartDateOffsets = Map[translateDateIntoOffset[#["startDate"]], interventionStartDates];
  (* endDate[i] = startDate[i+1] for 1 <= i < len, and endDate[len] = simulation end date *)
  (* interventionEndDateOffsets = Drop[Append[interventionStartDateOffsets, tmax0], 1]; *)

(*
  (* policy distancing filled with 1s to complete a full year *)
  fullDistancing = Join[
    (* pre-policy distancing - constant at 1 *)
    ConstantArray[1., IntegerPart[Min[dataDays]]],
    (* historical distancing data *)
    distancing,
    (* for any gap between the last day of data and today, fill in with the average of the last three days of data *)
    ConstantArray[Mean[distancing[[-3;;]]], today - IntegerPart[Max[dataDays]]],
    (* moving average going forward in the scenario *)
    ConstantArray[distancingLevel, scenario["distancingDays"]],
    (* post-policy distancing - constant at 1 *)
    ConstantArray[1.,
      totalDays - scenario["distancingDays"] - today]
  ];
*)
  {interventionStartDateOffsets, stateCode}
]
Print[translateInput["model/data/inputFile.json"]];