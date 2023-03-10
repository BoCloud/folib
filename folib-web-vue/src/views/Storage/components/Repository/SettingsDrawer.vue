<template>
  <a-drawer
    placement="right"
    width="65%"
    title="仓库设置"
    :visible="settingVisible"
    @close="settingDrawerClose"
    :zIndex="100"
  >
    <a-card
      :bordered="false"
      class="header-solid h-full card-profile-information"
      :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
      :headStyle="{ paddingRight: 0 }"
    >
      <a-tabs
        class="tabs-sliding"
        :default-active-key="1"
        :activeKey="settingTabActiveKey"
        @change="settingTabChange($event)"
      >
      <a-tab-pane :key="1" tab="权限设置">
        <Permission :folibRepository="this.folibRepository" :permissionForm="this.permissionForm" :userList="this.userList" :sourceUserList="this.sourceUserList" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Permission>
      </a-tab-pane>
      <a-tab-pane :key="2" tab="定时策略">
        <CronTask :folibRepository="this.folibRepository" @settingDrawerClose="settingDrawerClose"></CronTask>
      </a-tab-pane>
      </a-tabs>
    </a-card>
  </a-drawer>
</template>
<script>
import {
  repositoryEnableUsers,
  getRepositoryPermission,
} from "@/api/folib"
import Permission from '../Permission/index.vue'
import CronTask from "../Cron/index.vue"

export default {
  props: { 
		folibRepository: {
			type: Object,
			default: {},
		},
    settingVisible: {
			type: Boolean,
			default: false,
		},
	},
  data() {
    return {
      settingTabActiveKey: 1,
      permissionForm: {
        scope: 1,
        userList: []
		  },
      userList: [],
      sourceUserList: []
    }
  },
  components: {
    CronTask,
    Permission,
  },
  created() {
    this.initData()
  },
  mounted() {},
  watch: {
    settingVisible: function (val) {
      if (val) {
        this.initData()
      }
    },
  },
  methods: {
    initData() {
      this.getUsersList()
      this.queryRepositoryPermission()
    },
    settingTabChange(activeKey) {
      if (activeKey === 1) {
        this.initData()
      }
      this.settingTabActiveKey = activeKey
    },
    settingDrawerClose() {
      this.$emit('settingDrawerClose')
    },
    getUsersList() {
      repositoryEnableUsers({storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id}).then(res => {
        this.userList = res
      })
    },
    queryRepositoryPermission() {
      getRepositoryPermission({storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id}).then(res => {
        this.permissionForm.scope = res.scope
        if (res.userList && res.userList.length > 0) {
          this.permissionForm.userList = res.userList
          this.sourceUserList = res.userList
        }
      })
    },
  },
};
</script>