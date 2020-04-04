import * as React from 'react';
import {getOverviewData, getStatesWithData} from '../lib/data';
import {Overview} from '../components';
import {Section} from '../components/content';
import {Home} from '../md';

export default function Index(props) {
  return (
    <>
      <Home />
      <Section>
        <Overview {...props} />
      </Section>
    </>
  );
}

export const getStaticProps = () => {
  const overview = getOverviewData();
  const states = getStatesWithData();

  return {
    props: {
      overview,
      states,
    },
  };
};
