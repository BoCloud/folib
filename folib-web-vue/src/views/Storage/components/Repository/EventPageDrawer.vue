<template>
  <a-drawer
    placement="right"
    width="65%"
    title="事件"
    :visible="eventPageVisible"
    @close="eventDrawerClose"
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
      <a-tab-pane :key="1" tab="分发/晋级记录">
      </a-tab-pane>
      <a-tab-pane :key="2" tab="审计日志">
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
import UnionRepository from "../UnionRepository/index.vue"

export default {
  props: { 
		folibRepository: {
			type: Object,
			default: {},
		},
    eventPageVisible: {
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
    UnionRepository,
  },
  created() {

  },
  mounted() {},
  watch: {
    settingVisible: function (val) {
      this.settingTabActiveKey = 1
    },
  },
  methods: {
    settingTabChange(activeKey) {
      this.settingTabActiveKey = activeKey
    },
    eventDrawerClose() {
      this.$emit('eventDrawerClose')
    },
  },
};
</script>