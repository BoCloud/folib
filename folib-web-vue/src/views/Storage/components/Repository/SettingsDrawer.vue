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
        <Permission :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Permission>
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
    }
  },
  components: {
    CronTask,
    Permission,
  },
  created() {

  },
  mounted() {},
  watch: {
    settingVisible: function (val) {
    },
  },
  methods: {
    settingTabChange(activeKey) {
      this.settingTabActiveKey = activeKey
    },
    settingDrawerClose() {
      this.$emit('settingDrawerClose')
    },
  },
};
</script>