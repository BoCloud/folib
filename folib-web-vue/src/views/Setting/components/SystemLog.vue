<template>
  <div >
    <a-card class="header-solid"
          :bodyStyle="{ padding: '50px', height: '1000px', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
          <template #title>
            <a-row type="flex" align="middle">
              <a-col :span="24" :md="12">
                <h6 class="font-semibold m-0">在线日志查看</h6>
              </a-col>
              <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                <a-button type="primary" @click="viewLogs()">
                  同步
                </a-button>
              </a-col>
            </a-row>
          </template>
          <prism-editor class="my-editor" v-model="logs" :highlight="highlighterHandle" :line-numbers="false"
            :readonly="true"></prism-editor>
        </a-card>
  </div>
</template>
<script>

import { getMetrics, getMetricsHealth, viewLogs, getCassandraClusterInfo, cassandraRemoveNode } from "@/api/monitor";
// Importing charts
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'


export default {
 name:'SystemLog',
  data() {
    return {
      logs: "",
    
    };
  },
  components: {
    PrismEditor,
  },
  created() {

  },
  mounted() {
  },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },

    viewLogs() {
      viewLogs().then(res => {
        this.logs = res
      })
    }
  },

};
</script>

<style lang="scss" scoped>

</style>