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
        :default-active-key="1"
        :activeKey="settingTabActiveKey"
        @change="settingTabChange($event)"
      >
      <a-tab-pane :key="1" :tab="$t('Storage.PermissionSetting')">
        <Permission :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Permission>
      </a-tab-pane>
      <a-tab-pane :key="2" :tab="$t('Storage.TimingPolicy')">
        <CronTask :folibRepository="this.folibRepository" @settingDrawerClose="settingDrawerClose"></CronTask>
      </a-tab-pane>
      <a-tab-pane :key="3" :tab="$t('Storage.FederatedRepository')" v-if="this.folibRepository.type === 'hosted'">
        <UnionRepository :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></UnionRepository>
      </a-tab-pane>
      <a-tab-pane :key="4" :tab="$t('Storage.Scan')" v-if="(isAdmin() || (storageAdmin && storageAdmin === $store.state.user.name)) && this.folibRepository.type !== 'group' && this.folibRepository.layout !== 'HuggingFace' ">
        <Scan :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Scan>
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
import Permission from '../Permission/index.vue'
import CronTask from "../Cron/index.vue"
import UnionRepository from "../UnionRepository/index.vue"
import Scan from "../Scan/index.vue"

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
      storageAdmin: '',
    }
  },
  components: {
    CronTask,
    Permission,
    UnionRepository,
    Scan,
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
      this.settingTabActiveKey = 1
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
