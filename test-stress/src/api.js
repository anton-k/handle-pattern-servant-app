import http from 'k6/http';

// ---------------------------------------------------------------
// Public API methods

export const initApi = () => {
  return serviceApi(getRoutes);
};

// ---------------------------------------------------------------
// private funs

const save = (routes, message, tags) => {
  const payload = JSON.stringify({
    message: message,
    tags: tags,
  });

  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
  };
  http.post(routes.save, payload, params);
}

const getMessage = (routes, messageId) => {
  const url = `${routes.getId}/${messageId}`;
  http.get(url);
};

const getTag = (routes, tag) => {
  const url = `${routes.getTag}/${tag}`;
  http.get(url);
};

const toggleLogs = (routes) => {
  const url = routes.toggleLogs;
  const payload = JSON.stringify({});
  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
  };
  http.post(url, payload, params);
};

const serviceApi = (routes) => {
  const res = 
    {
      save: function (message, tags) { save(routes, message, tags); },
      getMessage: function (messageId) { getMessage(routes, messageId); },
      getTag: function (tag) { getTag(routes, tag); },
      toggleLogs: function () { toggleLogs(routes); },
      setupLogs: function () {
          if (__ENV.LOGS == 'false') {
            this.toggleLogs();
          };
        }
    }
  return res;
};

// ---------------------------------------------------------------
// routes

const localhost = port => `http://localhost:${port}`;

const route = (port, method) =>
  `${localhost(port)}/${method}`;

const readerPort = 7070;

const toRoute = method => 
  route(readerPort, `api/v1/${method}`);

const getRoutes = 
  { 
    save: toRoute('save'),
    getId: toRoute('get/message'),
    getTag: toRoute('list/tag'),
    toggleLogs: toRoute('toggle-logs')
  };

