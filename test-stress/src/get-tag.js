import { initApi } from './api.js';

const api = initApi();

const size = 1000;
const counter_size = 250;
var counter = 1;

const toTag = (n) => `tag-${n}`;

function bump_counter() {
  counter = 1 + ((counter + 1) % counter_size);
};

export function setup() {
  api.setupLogs();
  for (let i = 0; i < size; i++) {
    api.save('waiting for the summer', [toTag(i % counter_size)]);
  }
};

export default function () {
  bump_counter();
  api.getTag(toTag(counter));
};

