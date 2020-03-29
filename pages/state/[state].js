import React from 'react';
import {useRouter} from 'next/router';
import dayjs from 'dayjs';
import numeral from 'numeral';
import Link from 'next/link';

import {
  Controls,
  DistancingGraph,
  Layout,
  Line,
  OccupancyGraph,
  PopulationGraph,
  Section,
} from '../../components';
import {getStateData, getStatesWithData} from '../../lib/data';
import STATES from '../../lib/states';

const {useCallback, useState} = React;

const dayInMs = 24 * 60 * 60 * 1000;
const dayZero = new Date('Dec 31, 2019').getTime();
const dayToDate = (day) => new Date(dayZero + dayInMs * day);

const getDate = ({day}) => dayToDate(day);
const getDistancing = ({distancing}) => distancing;
const getProjectedPcr = ({projectedPcr}) => projectedPcr;
const getProjectedCurrentlyInfected = ({projectedCurrentlyInfected}) =>
  projectedCurrentlyInfected;
const getProjectedCurrentlyInfectious = ({projectedCurrentlyInfectious}) =>
  projectedCurrentlyInfectious;
const getProjectedDeaths = ({projectedDeaths}) => projectedDeaths;
const getProjectedCurrentlyHospitalized = ({projectedCurrentlyHospitalized}) =>
  projectedCurrentlyHospitalized;
const getProjectedCurrentlyCritical = ({projectedCurrentlyCritical}) =>
  projectedCurrentlyCritical;

export default ({data, states}) => {
  const {
    query: {state},
    push,
  } = useRouter();
  console.log('State', state, data);
  const [scenario, setScenario] = useState('scenario1');

  if (!data) {
    return <Layout noPad>Missing data for {state}</Layout>;
  }

  const scenarioSummary = data[scenario].summary;

  const hospitalCapacity = (1 - data.bedUtilization) * data.staffedBeds;

  const handleStateSelect = (e) => {
    push(`/state/${e.target.value}`);
  };

  return (
    <Layout>
      <style jsx>{`
        .controls {
          position: sticky;
          top: 0;
          background: white;
          padding: var(--spacing-02) 0;
          box-shadow: 0 2px 2px rgba(0, 0, 0, 0.2);
        }
      `}</style>
      <div className="flex flex-col justify-center">
        <div className="controls">
          <Section>
            <Controls
              state={state}
              states={states}
              scenario={scenario}
              setScenario={setScenario}
            />
          </Section>
        </div>
        <div className="flex flex-col px-6">
          <div className="mb-4 bg-white rounded overflow-hidden shadow-lg px-6 py-4">
            <div className="font-bold text-xl mb-2">Model Inputs</div>
            <div className="flex flex-around flex-col md:flex-row">
              <div className="w-full md:w-1/2 sm:mr-0 md:mr-10">
                <div>
                  <div className="text-gray-600 mb-2">Social Distancing</div>
                  <div className="text-gray-800 text-sm mb-4">
                    On the left axis social distance of 100% means no contact
                    with others, which yields an R0 (basic reproduction number)
                    for the virus of zero, since it cannot find new hosts. The
                    zero-percent distance is the un-inhibited reproduction
                    number which is thought to be around 3.1.
                  </div>
                  <DistancingGraph
                    scenario={scenario}
                    data={data}
                    x={getDate}
                    y={getDistancing}
                    xLabel="Social distance"
                    width="500"
                    height="300"
                  />
                </div>
              </div>
              <div className="w-full md:w-1/2">
                <div>
                  <div className="text-gray-600 mb-2">
                    Demographic Parameters
                  </div>
                  <div className="text-gray-800 text-sm mb-4">
                    Demographic parameters are calculated based on publically
                    available data on age distributions and hospital capacity.
                    The hospitalization probabilities are computed based on
                    assumed age-based need and state age distributions.
                  </div>
                  <table className="table-fixed mb-0 mb-4 border-2 border-gray-600">
                    <tbody>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Population
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(data.Population).format('0,0')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          ICU Beds
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(data.icuBeds).format('0,0')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Available Hospital Beds
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(
                            data.staffedBeds * (1 - data.bedUtilization)
                          ).format('0,0')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Probability of Not needing hospitalization
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(data.pS).format('0.00%')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Probability of needing hospitalization wihtout ICU
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(data.pH).format('0.00%')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Probability of needing ICU care
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(data.pC).format('0.00%')}
                        </td>
                      </tr>
                    </tbody>
                  </table>
                  <div>
                    <div className="text-gray-600 mb-2">
                      Model-fit Parameters
                    </div>
                    <div className="text-gray-800 text-sm mb-4">
                      Most parameters{' '}
                      <Link href="/about">
                        <a className="text-blue-700 hover:text-blue-500 leading-relaxed font-medium mb-8">
                          were fit
                        </a>
                      </Link>{' '}
                      on country data, but we adjust the following parameters on
                      a per-state basis for a more accurate fit.
                    </div>
                    <table className="table-fixed border-2 border-gray-600">
                      <tbody>
                        <tr>
                          <td className="font-semibold border px-4 py-2">
                            Import Date
                          </td>
                          <td className="border px-4 py-2">
                            {dayjs('2020-01-01')
                              .add(data.importtime - 1, 'day')
                              .format('MMM DD, YYYY')}
                          </td>
                        </tr>
                        <tr>
                          <td className="font-semibold border px-4 py-2">
                            Basic Reproduction Number (R0)
                          </td>
                          <td className="border px-4 py-2">
                            {numeral(data.R0).format('0.00')}
                          </td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="bg-white rounded overflow-hidden shadow-lg px-6 py-4 mb-4">
            <div className="font-bold text-xl mb-2">Model Predictions</div>
            {scenarioSummary['Date Contained'] === 'Not Contained' ? (
              <div className="mb-2 bg-red-200 border border-red-400 text-red-700 px-4 py-3 rounded">
                <strong className="font-bold">Virus not contained{` `}</strong>
                <span className="block m:inline">
                  {numeral(scenarioSummary['Population Infected']).format(
                    '0.00%'
                  )}{' '}
                  of state infected,{' '}
                  {numeral(scenarioSummary['Deaths']).format('0,0')} deaths
                </span>
              </div>
            ) : (
              <div className="mb-2 bg-green-200 border border-green-400 text-green-700 px-4 py-3 rounded">
                <strong className="font-bold">Virus contained{` `}</strong>
                <span className="block m:inline">
                  Virus contained on{' '}
                  {dayjs(scenarioSummary['Date Contained']).format(
                    'MMM D, YYYY'
                  )}
                </span>
              </div>
            )}
            {scenarioSummary['Date Hospitals Over Capacity'] !==
            'Hospitals Under capacity' ? (
              <div className="mb-2 bg-red-200 border border-red-400 text-red-700 px-4 py-3 rounded">
                <strong className="font-bold">Hospitals Overloaded{` `}</strong>
                <span className="block m:inline">
                  on{' '}
                  {dayjs(
                    scenarioSummary['Date Hospitals Over Capacity']
                  ).format('MMM D, YYYY')}
                </span>
              </div>
            ) : (
              <div className="mb-2 bg-green-200 border border-green-400 text-green-700 px-4 py-3 rounded">
                <strong className="font-bold">
                  Hospitals Under Capacity{` `}
                </strong>
              </div>
            )}
            {scenarioSummary['Date ICU Over Capacity'] !==
            'ICU Under capacity' ? (
              <div className="mb-2 bg-red-200 border border-red-400 text-red-700 px-4 py-3 rounded">
                <strong className="font-bold">ICU Overloaded{` `}</strong>
                <span className="block m:inline">
                  on{' '}
                  {dayjs(scenarioSummary['Date ICU Over Capacity']).format(
                    'MMM D, YYYY'
                  )}
                </span>
              </div>
            ) : (
              <div className="mb-2 bg-green-200 border border-green-400 text-green-700 px-4 py-3 rounded">
                <strong className="font-bold">ICU under capacity</strong>
              </div>
            )}

            <div className="flex flex-col md:flex-row">
              <div className="w-full md:w-1/2 md:mr-10">
                <div>
                  <div className="text-gray-600 mb-2">
                    Case Progression Curve
                  </div>
                  <div className="text-gray-800 text-sm mb-4">
                    We show the current number of infected and infectious
                    individuals as well as the cumulative number of expected PCR
                    confirmations. If less than 20% of the population is
                    infected and the number of active infections is reduced to a
                    small fraction of the population we consider the epidemic
                    contained, and place a grey box on the plot.
                  </div>
                  <PopulationGraph
                    scenario={scenario}
                    data={data}
                    x={getDate}
                    xLabel="Case progression curve"
                    width="500"
                    height="300"
                  >
                    <Line y={getProjectedCurrentlyInfected} stroke="#0670de" />
                    <Line
                      y={getProjectedCurrentlyInfectious}
                      stroke="#228403"
                    />
                    <Line y={getProjectedPcr} stroke="#ed6804" />
                  </PopulationGraph>
                </div>
              </div>
              <div className="w-full md:w-1/2">
                <div>
                  <div className="text-gray-600 mb-2">Projected Deaths</div>
                  <div className="text-gray-800 text-sm mb-4">
                    We project the cumulative number of deaths on a logarithmic
                    scale. Black dots are confirmed counts.
                  </div>
                  <PopulationGraph
                    scenario={scenario}
                    data={data}
                    x={getDate}
                    xLabel="Projected deaths"
                    width="500"
                    height="300"
                  >
                    <Line y={getProjectedDeaths} stroke="#0670de" />
                  </PopulationGraph>
                </div>
              </div>
            </div>
            <div className="flex flex-col md:flex-row">
              <div className="w-full md:w-1/2 md:mr-10">
                <div>
                  <div className="text-gray-600 mb-2">Hospital Occupancy</div>
                  <div className="text-gray-800 text-sm mb-4">
                    We estimate the hospital capacity for COVID-19 patients by
                    taking the number of available beds and discounting for that
                    hospital system's typical occupancy rate.
                  </div>
                  <OccupancyGraph
                    scenario={scenario}
                    data={data}
                    x={getDate}
                    y={getProjectedCurrentlyHospitalized}
                    cutoff={hospitalCapacity}
                    xLabel="Hospital occupancy"
                    cutoffLabel="Hospital capacity"
                    width="500"
                    height="300"
                  />
                </div>
              </div>
              <div className="w-full md:w-1/2 md:mr-10">
                <div>
                  <div className="text-gray-600 mb-2">ICU Occupancy</div>
                  <div className="text-gray-800 text-sm mb-4">
                    Note: we assign a higher probability of fatality in the case
                    the ICU capacity is over-shot. This can be seen in countries
                    like Italy where the fatlity rate is substantially higher
                    even controlling for the age distriubtion.
                  </div>
                  <OccupancyGraph
                    scenario={scenario}
                    data={data}
                    x={getDate}
                    y={getProjectedCurrentlyCritical}
                    cutoff={data.icuBeds}
                    xLabel="ICU occupancy"
                    cutoffLabel="ICU capacity"
                    width="500"
                    height="300"
                  />
                </div>
              </div>
            </div>
            <div className="flex flex-col md:flex-row">
              <div className="w-full md:w-1/2 md:mr-10">
                <div>
                  <div className="text-gray-600 mb-2">Outcome Summary</div>
                  <div className="text-gray-800 text-sm mb-4">
                    Fatality rate and percent of population infected are the
                    expected PCR confirmed rates with current levels of testing
                    in the US. The infected percentage is expected to be a few
                    times lower than the real rate and the real fatality rate a
                    few times lower to account for unconfirmed mild cases.
                  </div>
                  <table className="table-fixed mb-0 md:mb-4 border-2 border-gray-600">
                    <tbody>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Deaths
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(scenarioSummary.Deaths).format('0,0')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          PCR Confirmed
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(scenarioSummary['PCR Confirmed']).format(
                            '0,0'
                          )}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Percent of Population Infected
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(
                            scenarioSummary['Population Infected']
                          ).format('0.00%')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Fatality Rate
                        </td>
                        <td className="border px-4 py-2">
                          {numeral(scenarioSummary['Fatality Rate']).format(
                            '0.00%'
                          )}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Date Contained
                        </td>
                        <td className="border px-4 py-2">
                          {scenarioSummary['Date Contained'] === 'Not Contained'
                            ? 'Not Contained'
                            : dayjs(scenarioSummary['Date Contained']).format(
                                'MMM D, YYYY'
                              )}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Date Hospitals Overloaded
                        </td>
                        <td className="border px-4 py-2">
                          {scenarioSummary['Date Hospitals Over Capacity'] ===
                          'ICU Under capacity'
                            ? 'ICU Under capacity'
                            : dayjs(
                                scenarioSummary['Date Hospitals Over Capacity']
                              ).format('MMM D, YYYY')}
                        </td>
                      </tr>
                      <tr>
                        <td className="font-semibold border px-4 py-2">
                          Date ICU Overloaded
                        </td>
                        <td className="border px-4 py-2">
                          {scenarioSummary['Date ICU Over Capacity'] ===
                          'ICU Under capacity'
                            ? 'ICU Under capacity'
                            : dayjs(
                                scenarioSummary['Date ICU Over Capacity']
                              ).format('MMM D, YYYY')}
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </Layout>
  );
};

export const getStaticProps = ({params: {state}}) => {
  const data = getStateData(state);

  return {
    props: {
      data,
      states: getStatesWithData(),
    },
  };
};

export const getStaticPaths = (_ctx) => {
  return {
    paths: getStatesWithData().map((state) => ({
      params: {state},
    })),
    fallback: false,
  };
};
