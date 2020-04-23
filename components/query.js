import {wrap} from 'optimism';
import {useModelState, DistributionSeriesFullFragment} from './modeling';
import {fetchSuspendable} from '../lib/fetch';

const toQueryString = (
  location,
  scenario,
  [type, query, transforms, fragments = []]
) => {
  let body = query;
  switch (type) {
    case 'Location':
      body = `{
        location(id: "${location}") ${query}
      }`;
      break;
    case 'Scenario':
      body = `{
        location(id: "${location}") {
          scenario(id: "${scenario}") ${query}
        }
      }`;
      break;
  }
  return Array.from(new Set([...fragments, body])).join('\n');
};

export const seriesProps = ['data', 'empty', 'max', 'min'];

export const compactDistributionProps = [
  'expected',
  'expectedTestTrace',
  'confirmed',
  'percentile10',
  'percentile50',
  'percentile90',
];

export const fullDistributionProps = [
  ...compactDistributionProps,
  'percentile20',
  'percentile30',
  'percentile40',
  'percentile60',
  'percentile70',
  'percentile80',
];

export const SeriesFull = [
  `fragment SeriesFull on Series {
    data
    empty
    max
    min
  }`,
];

export function mapBlock(propNames, block) {
  return propNames
    .map(
      (propName) => `${propName} {
        ${block}
      }`
    )
    .join('\n');
}

export const DistributionSeriesFull = [
  ...SeriesFull,
  `fragment DistributionSeriesFull on DistributionSeries {
    ${mapBlock(fullDistributionProps, '...SeriesFull')}
  }`,
];

export function createSeries(accessor) {
  const transforms = {};
  seriesProps.map((prop) => {
    transforms[prop] = (data) => {
      const series = accessor(data);
      return series ? series[prop] : null;
    };
  });
  transforms.get = (data, i) => {
    const list = transforms.data(data);
    return list ? list[i] : null;
  };
  return transforms;
}

export function createDistributionSeries(accessor) {
  const transforms = {};
  fullDistributionProps.map((prop) => {
    transforms[prop] = createSeries((data) => {
      const distribution = accessor(data);
      return distribution ? distribution[prop] : null;
    });
  });
  return transforms;
}

const queries = [
  [
    'Query',
    `{
      locations {
        id
        name
      }
    }`,
    {locations: ({locations}) => locations},
  ],
  [
    'Location',
    `{
      dateModelRun
      days { data }
      icuBeds
      importtime
      population
      r0
      scenarios {
        id
        name
        distancingLevel
        distancingDays
      }
    }`,
    {
      dateModelRun: (location) => location.dateModelRun,
      days: ({days}) => days.data,
      icuBeds: (location) => location.icuBeds,
      importtime: (location) => location.importtime,
      population: (location) => location.population,
      r0: (location) => location.r0,
      scenarios: (location) => location.scenarios,
    },
  ],
  [
    'Scenario',
    `{
      dateContained
      distancing { ...DistributionSeriesFull }
      rt { ...DistributionSeriesFull }
    }`,
    {
      dateContained: (d) => d.dateContained,
      distancing: createDistributionSeries((d) => d.distancing),
      rt: createDistributionSeries((d) => d.rt),
    },
    DistributionSeriesFull,
  ],
  [
    'Scenario',
    `{
      cumulativeDeaths { ...DistributionSeriesFull }
      cumulativeRecoveries { ...DistributionSeriesFull }
      currentlyInfected { ...DistributionSeriesFull }
      currentlyInfectious { ...DistributionSeriesFull }
      susceptible { ...DistributionSeriesFull }
      cumulativeExposed { ...DistributionSeriesFull }
      cumulativePcr { ...DistributionSeriesFull }
    }`,
    {
      cumulativeDeaths: createDistributionSeries((d) => d.cumulativeDeaths),
      cumulativeRecoveries: createDistributionSeries(
        (d) => d.cumulativeRecoveries
      ),
      currentlyInfected: createDistributionSeries((d) => d.currentlyInfected),
      currentlyInfectious: createDistributionSeries(
        (d) => d.currentlyInfectious
      ),
      susceptible: createDistributionSeries((d) => d.susceptible),
      cumulativeExposed: createDistributionSeries((d) => d.cumulativeExposed),
      cumulativePcr: createDistributionSeries((d) => d.cumulativePcr),
    },
    DistributionSeriesFull,
  ],
  [
    'Scenario',
    `{
      dailyDeath { ...DistributionSeriesFull }
      newlyExposed { ...DistributionSeriesFull }
      dailyPcr { ...DistributionSeriesFull }
    }`,
    {
      dailyDeath: createDistributionSeries((d) => d.dailyDeath),
      newlyExposed: createDistributionSeries((d) => d.newlyExposed),
      dailyPcr: createDistributionSeries((d) => d.dailyPcr),
    },
    DistributionSeriesFull,
  ],
  [
    'Scenario',
    `{
      hospitalCapacity { ...SeriesFull }
      currentlyHospitalized { ...DistributionSeriesFull }
      currentlyReportedHospitalized { ...DistributionSeriesFull }
      cumulativeHospitalized { ...DistributionSeriesFull }
      cumulativeReportedHospitalized { ...DistributionSeriesFull }
    }`,
    {
      hospitalCapacity: createSeries((d) => d.hospitalCapacity),
      currentlyHospitalized: createDistributionSeries(
        (d) => d.currentlyHospitalized
      ),
      currentlyReportedHospitalized: createDistributionSeries(
        (d) => d.currentlyReportedHospitalized
      ),
      cumulativeHospitalized: createDistributionSeries(
        (d) => d.cumulativeHospitalized
      ),
      cumulativeReportedHospitalized: createDistributionSeries(
        (d) => d.cumulativeReportedHospitalized
      ),
    },
    [...SeriesFull, ...DistributionSeriesFull],
  ],
  [
    'Scenario',
    `{
      currentlyCritical { ...DistributionSeriesFull }
      cumulativeCritical { ...DistributionSeriesFull }
    }`,
    {
      currentlyCritical: createDistributionSeries((d) => d.currentlyCritical),
      cumulativeCritical: createDistributionSeries((d) => d.cumulativeCritical),
    },
    DistributionSeriesFull,
  ],
  [
    'Location',
    `{
      domain {
        rt { expected { max }}
        cumulativeDeaths { expected { max } }
        currentlyInfected { expected { max } }
        currentlyInfectious { expected { max } }
        dailyDeath { expected { max } }
        newlyExposed { expected { max } }
        dailyPcr { expected { max } }
        currentlyHospitalized { expected { max } }
        cumulativeHospitalized { expected { max } }
        currentlyCritical { expected { max } }
        cumulativeCritical { expected { max } }
      }
    }`,
    {
      domain: {
        rt: ({domain}) => domain.rt.expected.max,
        seir: ({domain}) =>
          domain.cumulativeDeaths.expected.max +
          domain.currentlyInfected.expected.max +
          domain.currentlyInfectious.expected.max,
        cumulativeDeaths: ({domain}) => domain.cumulativeDeaths.expected.max,
        dailyDeath: ({domain}) => domain.dailyDeath.expected.max,
        newlyExposed: ({domain}) => domain.newlyExposed.expected.max,
        hospitalized: {
          currently: ({domain}) => domain.currentlyHospitalized.expected.max,
          cumulative: ({domain}) => domain.cumulativeHospitalized.expected.max,
        },
        critical: {
          currently: ({domain}) => domain.currentlyCritical.expected.max,
          cumulative: ({domain}) => domain.cumulativeCritical.expected.max,
        },
      },
    },
  ],
  [
    'Location',
    `{
      parameters {
        id
        name
        value
        description
        type
      }
    }`,
    {
      parameters: (location) => location.parameters,
    },
  ],
];

function getTypeAccessor(type, request) {
  switch (type) {
    case 'Location':
      return () => request().location;
    case 'Scenario':
      return () => request().location.scenario;
    case 'Query':
    default:
      return request;
  }
}

function bindTransforms(transforms, accessor) {
  const result = {};
  for (let [key, transform] of Object.entries(transforms)) {
    if (typeof transform === 'function') {
      result[key] = (...args) => transform(accessor(), ...args);
    } else if (typeof transform === 'object') {
      result[key] = bindTransforms(transform, accessor);
    }
  }
  return result;
}

export const fetchLocationData = wrap(function fetchLocationData(
  location,
  scenario
) {
  const requests = {};
  for (let query of queries) {
    const [type, queryString, transforms] = query;
    const request = fetchSuspendable(toQueryString(location, scenario, query));
    const typeAccessor = getTypeAccessor(type, request);

    Object.assign(requests, bindTransforms(transforms, typeAccessor));
  }
  return requests;
});
