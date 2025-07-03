<template>
  <a-drawer
    placement="right"
    width="65%"
    :title="$t('Storage.WarehouseSetup')"
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
        :default-active-key="2"
        :activeKey="settingTabActiveKey"
        @change="settingTabChange($event)"
      >
      <a-tab-pane :key="2" :tab="$t('Storage.TimingPolicy')">
        <CronTask v-if="settingTabActiveKey === 2" :folibRepository="this.folibRepository" @settingDrawerClose="settingDrawerClose"></CronTask>
      </a-tab-pane>
      </a-tabs>
    </a-card>
  </a-drawer>
</template>
<script>
import {
  getStorageAndRepositoryPermission
} from "@/api/folib"
import { hasRole, isAdmin, isAnonymous, isLogin } from '@/utils/permission'
import CronTask from "../Cron/index.vue"
import UnionRepository from "../UnionRepository/index.vue"

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
      settingTabActiveKey: 2,
      storageAdmin: '',
    }
  },
  components: {
    CronTask,
    UnionRepository
  },
  created() {

  },
  mounted() {
      // console.log("user:",this.$store.state.user.name)
      // console.log('folibRepository:', this.folibRepository);
      // console.log('settingVisible:', this.settingVisible);
  },
  watch: {
    settingVisible: function (val) {
      this.initData()
    },
  },
  methods: {
    initData() {
      this.settingTabActiveKey = 2
      this.queryStorageAdmin()
    },
    settingTabChange(activeKey) {
      this.settingTabActiveKey = activeKey
    },
    settingDrawerClose() {
      this.$emit('settingDrawerClose')
    },
    isAdmin() {
      return isAdmin()
    },
    queryStorageAdmin () {
      this.storageAdmin = ''
      getStorageAndRepositoryPermission(
        this.folibRepository.storageId,
        this.folibRepository.id
      ).then(res => {
        this.storageAdmin = res.storageAdmin
      })
    },
  },
};
</script>
