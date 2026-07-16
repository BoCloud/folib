import { axios } from "@/utils/request";

const preApi = "/dependency";

// 获取组件数据列表
export function getLicenseGroup(data) {
  return axios({
    url: `${preApi}/v1/licenseGroup`,
    method: "get",
    params: data,
  });
}

// 修改证书组名称
//
export function editLicenseGroup(data) {
  return axios({
    url: `${preApi}/v1/licenseGroup`,
    method: "post",
    data: data,
  });
}
// 删除证书组
export function deleteLicenseGroup(data) {
  return axios({
    url: `${preApi}/v1/licenseGroup/${data}`,
    method: "delete",
  });
}
// 新增证书组
export function addLicenseGroup(data) {
  return axios({
    url: `${preApi}/v1/licenseGroup`,
    method: "put",
    data: data,
  });
}

// 证书列表
export function getLicense(data) {
  return axios({
    url: `${preApi}/v1/license/concise`,
    method: "get",
    params: data,
  });
}
// 选择证书
export function selectLicense(o, n) {
  return axios({
    url: `${preApi}/v1/licenseGroup/${o}/license/${n}`,
    method: "post",
  });
}

// 删除单个证书
export function deleteLicense(o, n) {
  return axios({
    url: `${preApi}/v1/licenseGroup/${o}/license/${n}`,
    method: "delete",
  });
}

/**
 * 政策相关
 */

// 查询政策列表
export function getPolicyList(data) {
  return axios({
    url: `${preApi}/v1/policy`,
    method: "get",
    params: data,
  });
}

// 证书
export function getPolicyLicenceList(data) {
  return axios({
    url: `${preApi}/v1/license`,
    method: "get",
    params: data,
  });
}

// 证书组
export function getPolicyLicenceGroupList() {
  return axios({
    url: `${preApi}/v1/licenseGroup?limit=9999`,
    method: "get",
  });
}

// 修改政策信息
export function addPolicyList(id, data) {
  return axios({
    url: `${preApi}/v1/policy/${id}/condition`,
    method: "put",
    data: data,
  });
}

// 新增政策信息
export function editPolicyList(data) {
  return axios({
    url: `${preApi}/v1/policy/condition`,
    method: "post",
    data: data,
  });
}

// 修改政策名
export function postPolicyList(data) {
  return axios({
    url: `${preApi}/v1/policy`,
    method: "post",
    data: data,
  });
}

// 删除政策
export function delPolicyList(data) {
  return axios({
    url: `${preApi}/v1/policy/condition/${data}`,
    method: "delete",
  });
}
