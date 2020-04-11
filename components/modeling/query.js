import {useMemo} from 'react';
import useSWR from 'swr';
import {useModelData} from './useModelData';

export function useLocationQuery(query) {
  const {state} = useModelData();
  const {data, error} = useSWR(`{ location(id: "${state}") ${query} }`);
  return [data?.location, error];
}

export function useScenarioQuery(query) {
  const {scenario} = useModelData();
  const [data, error] = useLocationQuery(
    `{ scenario(id: "${scenario}") ${query} }`
  );
  return [data?.scenario, error];
}

export function useDistributionQuery(distribution, query) {
  const [data, error] = useScenarioQuery(`{ ${distribution} ${query} }`);
  return [(data && data[distribution]) || null, error];
}

const compactSeries = [
  'expected',
  'confirmed',
  'percentile10',
  'percentile50',
  'percentile90',
];

const fullSeries = [
  ...compactSeries,
  'percentile20',
  'percentile30',
  'percentile40',
  'percentile60',
  'percentile70',
  'percentile80',
];

export function useDistribution(distribution, series = fullSeries) {
  const [data, error] = useDistributionQuery(
    distribution,
    `{ ${series.join('\n')} }`
  );
  const fns = useMemo(() => {
    if (!data) {
      return;
    }
    const result = {};
    for (let [key, series] of Object.entries(data)) {
      result[key] = (_, i) => series[i];
    }
    return result;
  }, [data]);
  return [fns, error];
}
