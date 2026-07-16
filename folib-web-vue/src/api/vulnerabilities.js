import { axios } from "@/utils/request";

// 获取漏洞数据列表
export function getVulnerabilitiesList(data) {
  return axios({
    url: `/api/vulnerabilityDatabase/page`,
    method: "get",
    params: data,
  });
}

// 获取详情头部数据
export function getVulnerabilityDetail(cve) {
  return axios({
    url: `/api/vulnerabilityDatabase/detail/${cve}`,
    method: "get",
  });
}

// 获取受影响列表数据
export function getVulnerabilityDetailData(data) {
  return axios({
    url: `/api/vulnerabilityDatabase/artifactPage`,
    method: "get",
    params: data,
  });
}
