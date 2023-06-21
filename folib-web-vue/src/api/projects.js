import { axios } from "@/utils/request";

const preApi = "/dependency";

// 获取项目数据列表
export function getProjectsList(data) {
  return axios({
    url: `${preApi}/v1/project`,
    method: "get",
    params: data,
  });
}

// 获取详情头部数据
export function getProjectsHeaderDetail(data) {
  return axios({
    url: `${preApi}/v1/project/${data}`,
    method: "get",
  });
}

// 获取头部echarts数据
export function getProjectsCurrent(data) {
  return axios({
    url: `${preApi}/v1/metrics/project/${data}/current`,
    method: "get",
  });
}

// 获取详情数据版本
export function getProjectVersions(data) {
  return axios({
    url: `${preApi}/v1/project?offset=0&limit=10&excludeInactive=true&name=` + encodeURIComponent(data),
    method: "get",
  });
}

// 项目漏洞
export function getProjectsVulnerabilities(data) {
  return axios({
    url: `${preApi}/v1/metrics/project/${data}/days/90`,
    method: "get",
  });
}

// // 项目组件
// export function getProjectsComponents(id, data) {
//   return axios({
//     url: `${preApi}/v1/component/project/${id}`,
//     method: "get",
//     params: data,
//   });
// }

// 项目组件
export function getProjectsComponents(data) {
  return axios({
    url: `/api/component/page`,
    method: "get",
    params: data,
  });
}

export function getProjectsComponentsByArtifact(data) {
  return axios({
    url: `/api/component/pageByArtifact`,
    method: "get",
    params: data,
  });
}

// 项目组件
export function getProjectsService(id, data) {
  return axios({
    url: `${preApi}/v1/service/project/${id}`,
    method: "get",
    params: data,
  });
}

// 漏洞审查
export function getFindingProject(id, data) {
  return axios({
    url: `${preApi}/v1/finding/project/${id}`,
    method: "get",
    params: data,
  });
}

// 漏洞列表
export function getVulnerability(data) {
  return axios({
    url: `/api/vulnerability/pageByArtifact`,
    method: "get",
    params: data,
  });
}

// 漏洞利用预测
export function getFindingProjectNVD(id, data) {
  return axios({
    url: `${preApi}/v1/finding/project/${id}`,
    method: "get",
    params: data,
  });
}

// 违反政策
export function getViolationProjects(id, data) {
  return axios({
    url: `${preApi}/v1/violation/project/${id}`,
    method: "get",
    params: data,
  });
}
