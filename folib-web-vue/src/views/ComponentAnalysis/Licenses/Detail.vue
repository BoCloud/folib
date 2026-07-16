<template>
  <div class="wrapper">
    <template>
      <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
        <a-tab-pane key="1" :tab="$t('Licenses.Overview')">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: '24px' }">
            <table>
              <tr>
                <td class="heading">{{ $t('Licenses.LicenseName') }}:</td>
                <td>{{ license.licenseName }}</td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.SPDXLicenseID') }}:</td>
                <td>{{ license.licenseId }}</td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.OSIAuthorized') }}:</td>
                <td>
                  <span v-if="license.isOsiApproved"><a-icon type="check-square" /></span>
                  <span v-else><a-icon type="close-square" /></span>
                </td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.FreeSoftware') }}:</td>
                <td>
                  <span v-if="license.isFsfLibre"><a-icon type="check-square" /></span>
                  <span v-else><a-icon type="close-square" /></span>
                </td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.Abandon') }}:</td>
                <td>
                  <span v-if="license.isDeprecated"><a-icon type="check-square" /></span>
                  <span v-else><a-icon type="close-square" /></span>
                </td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.CustomLicenses') }}:</td>
                <td>
                  <span v-if="license.isCustomLicense"><a-icon type="check-square" /></span>
                  <span v-else><a-icon type="close-square" /></span>
                </td>
              </tr>
              <tr>
                <td class="heading">{{ $t('Licenses.remark') }}:</td>
                <td>{{ license.comment }}</td>
              </tr>
              <tr>
                <td></td>
                <td><img v-if="license.isOsiApproved" src="@/assets/img/osi-logo.svg" alt="OSI logo" width="80" /></td>
              </tr>
            </table>
          </a-card>
        </a-tab-pane>
        <a-tab-pane key="2" :tab="$t('Licenses.originalLicenseText')">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: '24px' }">
            <prism-editor
                class="my-editor"
                v-model="license.content"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
          </a-card>
        </a-tab-pane>
        <a-tab-pane key="3" :tab="$t('Licenses.ChineseLicenseText')">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: '24px' }">
            <prism-editor
                class="my-editor"
                v-model="license.contentCn"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
          </a-card>
        </a-tab-pane>
        <a-tab-pane key="4" :tab="$t('Licenses.Template')">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: '24px' }">
            <prism-editor
                class="my-editor"
                v-model="license.template"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
          </a-card>
        </a-tab-pane>
        <a-tab-pane key="5" :tab="$t('Licenses.SourceHeader')">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: '24px' }">
            <prism-editor
                class="my-editor"
                v-model="license.header"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
          </a-card>
        </a-tab-pane>
      </a-tabs>
    </template>
  </div>
</template>

<script>
import { getLicenseDetail } from "@/api/licenses.js";
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css";
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
export default {
  components: {
    PrismEditor,
  },
  created() {
    this.getData();
  },
  data() {
    return {
      license: {},
    };
  },
  methods: {
    getData() {
      const licenseId = this.$route.params.id;
      getLicenseDetail(licenseId).then((res) => {
        this.license = res;
      });
    },
    handleChangeTabs() { },
    highlighterHandle(code) {
      return highlight(code, languages.js);
    },
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  width: 100%;
}

.heading {
  font-weight: bold;
  white-space: nowrap;
}

td {
  padding: 8px;
  text-align: left;
  vertical-align: text-top;
}

.formattedLicenseContent {
  font-size: 1.25em;
  font-family: monospace;
  white-space: pre-wrap;
}
</style>
