<template>
  <div class="wrapper">
    <a-card>
      <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
        <a-tab-pane key="1" tab="政策">
          <PolicyList v-if="tabActive == 1"></PolicyList>
        </a-tab-pane>
        <a-tab-pane key="2" tab="证书组">
          <LicenseGroupList v-if="tabActive == 2"></LicenseGroupList>
        </a-tab-pane>
      </a-tabs>
    </a-card>
  </div>
</template>

<script>
import LicenseGroupList from "./LicenseGroupList.vue";
import PolicyList from "./PolicyList.vue";
export default {
  components: { LicenseGroupList, PolicyList },
  data() {
    return {
      tabActive: 1,
    };
  },
  methods: {
    handleChangeTabs(val) {
      this.tabActive = val;
    },
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  margin-bottom: 20px;
}
</style>
