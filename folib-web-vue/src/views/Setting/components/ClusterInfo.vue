<template>
  <div >
  <a-row :gutter="24" type="flex" align="stretch">
          <a-col :span="24" class="text-left mb-10" v-if="repair">
            <a-button type="primary" @click="cassandraRepair()">
              数据修复
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
                    <a-card-meta title="节点信息" :description="'IP：' + item.endpoint">
                      <a-avatar slot="avatar" src="images/folib/cluster_node.svg" />
                    </a-card-meta>
                  </template>
                  <a-popconfirm slot="extra" title="危险操作！！！确定要移除此节点吗？" okType="danger" ok-text="确定" cancel-text="取消"
                    v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                    class="remove-node-popconfirm"
                    @confirm="removeNode(cassandraClusterInfo.hostIDMap[item.endpointWithPort.hostAddressAndPort], item.endpointWithPort.hostAddressAndPort)">
                    <a-icon type="poweroff" :style="{ fontSize: '24px', color: '#ff4d4f' }" />
                  </a-popconfirm>
                  <p class="text-dark">
                  </p>
                  <hr class="my-25">
                  <a-descriptions title="基本信息" :column="1">
                    <a-descriptions-item label="状态">
                      <a-badge
                        v-if="cassandraClusterInfo.liveNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                        color="#87d068" text="在线" />
                      <a-badge
                        v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                        color="#f50" text="离线" />
                    </a-descriptions-item>
                    <a-descriptions-item label="使用端口">
                      {{ item.endpointWithPort.port }}
                    </a-descriptions-item>
                    <a-descriptions-item label="数据表总数">
                      {{ cassandraClusterInfo.statsHolderMap.total_number_of_tables }}
                    </a-descriptions-item>
                    <a-descriptions-item label="数据所有权">
                      <span v-if="item.owns">{{ (item.owns * 100).toFixed(2) + '%' }}</span>
                      <span v-else>{{ '100.00%' }}</span>
                    </a-descriptions-item>
                    <a-descriptions-item label="预估数据量">
                      {{ cassandraClusterInfo.loadMap[item.endpointWithPort.hostAddressAndPort] }}
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
        message = "操作成功"
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
      this.successMsg("正在移除节点 " + endpoint + "，请稍候刷新页面查看")
      cassandraRemoveNode(hostId).then(res => {
        this.successMsg("成功移除节点：" + endpoint)
      }).catch((err) => {

      })
    },
    cassandraRepair() {
      cassandraRepair().then(res => {
        this.successMsg("集群数据修复完成");
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      })
    }
  },

};
</script>