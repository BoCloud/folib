<template>
  <div >
  <a-row :gutter="24" type="flex" align="stretch">
          <a-col :span="24" class="text-left mb-10" v-if="repair">
            <a-button type="primary" @click="cassandraRepair()">
              {{ $t('Setting.DataRepair') }}
            </a-button>
          </a-col>
          <a-col :span="24" :xl="24" class="mb-24" v-for="(dc, dcKey, dcIndex) in cassandraClusterInfo.dcsMap"
            :key="dcIndex">
            <a-row :gutter="24" type="flex" align="stretch">
              <a-col :span="12" :xl="6" class="mb-24" v-for="(item, index) in dc" :key="index">
                <a-card :bordered="false"
                  :class="item.endpointWithPort.hostAddressAndPort == cassandraClusterInfo.endpoint ? 'header-solid h-full card-profile-information current-node' : 'header-solid h-full card-profile-information'"
                  :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0, }">
                  <template #title>
                    <a-card-meta :title="$t('Setting.NodeInformation')" :description="'IP：' + item.endpoint">
                      <a-avatar slot="avatar" src="images/folib/cluster_node.svg" />
                    </a-card-meta>
                  </template>
                  <a-popconfirm slot="extra" :title="$t('Setting.DangerousOperation')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                    class="remove-node-popconfirm"
                    @confirm="removeNode(cassandraClusterInfo.hostIDMap[item.endpointWithPort.hostAddressAndPort], item.endpointWithPort.hostAddressAndPort)">
                    <a-icon type="poweroff" :style="{ fontSize: '24px', color: '#ff4d4f' }" />
                  </a-popconfirm>
                  <p class="text-dark">
                  </p>
                  <hr class="my-25">
                  <a-descriptions :title="$t('Setting.BasicInformation')" :column="1">
                    <a-descriptions-item :label="$t('Setting.Status')">
                      <a-badge
                        v-if="cassandraClusterInfo.liveNodeList.indexOf(item.endpointWithPort) != -1"
                        color="#87d068" :text="$t('Setting.Online')" />
                      <a-badge
                        v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort) != -1"
                        color="#f50" :text="$t('Setting.Offline')" />
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.UsingPorts')">
                      {{ parseAddressRegex(item.endpointWithPort).port }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.TotalNumberOfDataTables')">
                      {{ cassandraClusterInfo.statsHolderMap.total_number_of_tables }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.DataOwnership')">
                      <span v-if="item.owns">{{ (item.owns * 100).toFixed(2) + '%' }}</span>
                      <span v-else>{{ '100.00%' }}</span>
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.EstimatedDataSize')">
                      {{ cassandraClusterInfo.loadMap[item.endpointWithPort] }}
                    </a-descriptions-item>
                  </a-descriptions>
                </a-card>
              </a-col>
            </a-row>
          </a-col>
        </a-row>
  </div>
</template>
<script>

import {   getCassandraClusterInfo, cassandraRemoveNode, cassandraRepair } from "@/api/monitor";
// Importing charts
export default {
  data() {
    return {
      repair: false,
      cassandraClusterInfo: {
        endpoint: "",
        localHostId: "",
        hostIDMap: {},
        loadMap: {},
        statsHolderMap: {},
        tokenList: [],
        unreachableNodeList: [],
        joiningNodeList: [],
        liveNodeList: [],
        movingNodeList: [],
        leavingNodeList: [],
        ownershipMap: {},
        effectiveOwnershipMap: {},
        seedList: [],
        gossipInfo: "",
        dcsMap: {
          "FOLIB-DC1": [
            {
              endpoint: "",
              resolveIp: true,
              owns: undefined,
              token: "",
              endpointWithPort: {
                address: "",
                addressBytes: "",
                port: undefined,
                hostAddressAndPort: "",
                hostAddressAndPortForJMX: ""
              }
            }
          ]
        },
        tokensToEndpointsMap: {},
      },
    };
  },
  components: {
  },
  created() {
  this.queryCassandraClusterInfo()
  },
  mounted() {
  },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('Setting.OperationSuccessful')
      }
      this.$notification["success"]({
        message: message,
        description: ""
      })
    },
    queryCassandraClusterInfo() {
      getCassandraClusterInfo().then(res => {
        this.cassandraClusterInfo = res
        if (this.cassandraClusterInfo.hostIDMap) {
          let hostIDKeys = Object.keys(this.cassandraClusterInfo.hostIDMap)
          if (hostIDKeys.length > 1) {
            this.repair = true
          } else {
            this.repair = false
          }
        }
      })
    },

    removeNode(hostId, endpoint) {
      const key = 'removeNode_' + hostId
      const flag = Storage.prototype.getCanExpireLocal(key);
      if (flag) {
        console.log("=====>>>>>存在", key)
        return
      }
      Storage.prototype.setCanExpireLocal(key, true, 30 * 1000)
      this.successMsg(this.$t('Setting.RemovingNodes') + endpoint + this.$t('Setting.refreshThePageToCheck'))
      cassandraRemoveNode(hostId).then(res => {
        this.successMsg(this.$t('Setting.SuccessRemovedNode') + endpoint)
      }).catch((err) => {

      })
    },
    cassandraRepair() {
      cassandraRepair().then(res => {
        this.successMsg(this.$t('Setting.clusterDataIsRepaired'));
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      })
    },
      parseAddressRegex(str) {
          // 正则解释：匹配IPv4和端口（端口0-65535）
          const regex = /^((?:\d{1,3}\.){3}\d{1,3}):(0|(?:[1-9]\d{0,3}|[1-5]\d{4}|6[0-4]\d{3}|65[0-4]\d{2}|655[0-2]\d|6553[0-5]))$/;
          const match = str.match(regex);

          if (!match) {
              return { ip: null, port: null, error: '格式错误或端口超出范围（0-65535）' };
          }

          const ip = match[1];
          const port = parseInt(match[2], 10);

          // 校验IP每段是否为0-255（正则已限制整体格式，但需细化每段）
          const ipSegments = ip.split('.');
          const isIpValid = ipSegments.every(seg => {
              const num = parseInt(seg, 10);
              return num >= 0 && num <= 255;
          });

          if (!isIpValid) {
              return { ip, port, error: 'IP段无效（每段0-255）' };
          }

          return { ip, port, error: null };
      }
  },

};
</script>
