<template>
  <div>
    <a-row type="flex" :gutter="24">
      <!-- Platform Settings Column -->
      <a-col :span="24" :md="7" class="mb-24">
        <a-card style="max-height:1024px;min-height:454px;overflow-y: auto" class="header-solid"
          :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
          <template #title>
            <h6 class="font-semibold m-0">日志列表</h6>
          </template>
          <a-directory-tree :replaceFields="{
            key: 'name',
            title: 'name',
            children: 'children'
          }" :tree-data="treeData" :load-data="onLoadData" @select="treeSelect">
          </a-directory-tree>
        </a-card>
      </a-col>
      <a-col :span="24" :md="17" class="mb-40">
        <a-card class="header-solid"
          :bodyStyle="{ padding: '40px', height: '1000px', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
          <template #title>
            <a-row type="flex" align="middle">
              <a-col :span="24" :md="12">
                <h6 class="font-semibold m-0">在线日志查看</h6>
              </a-col>
              <a-col v-if="path" :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                <a-button type="primary" @click="viewLogs()">
                  同步
                </a-button>
              </a-col>
            </a-row>
          </template>
          <prism-editor class="my-editor" v-model="logs" :highlight="highlighterHandle" :line-numbers="false"
            :readonly="true"></prism-editor>
        </a-card>
      </a-col>
    </a-row>
  </div>
</template>
<script>

import { viewLogs, browseLogs } from "@/api/monitor";
// Importing charts
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'


export default {
  name: 'SystemLog',
  data() {
    return {
      logs: "",
      treeData: [],
      path: ""
    };
  },
  components: {
    PrismEditor,
  },
  created() {
    this.getLogs()
  },
  mounted() {
  },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },
    viewLogs() {
      this.logs = ""
      viewLogs(this.path).then(res => {
        this.logs = res
      })
    },
    getLogs() {
      browseLogs().then(
        res => {
          const d = res.directories
          d.forEach((item, index, d) => {
            item.type = 'dir'
          })
          const f = res.files
          f.forEach((item, index) => {
            item.isLeaf = true
            item.type = 'file'
          })
          this.treeData = d.concat(f)
        }
      ).catch((err) => {
      })
    },
    onLoadData(treeNode) {
      return new Promise(resolve => {
        if (treeNode.dataRef.children) {
          resolve()
          return
        }
        let path = ''
        if (treeNode.dataRef && treeNode.dataRef.type === 'dir') {
          path = treeNode.dataRef.name
        }
        browseLogs(path).then(res => {
          if (!treeNode.dataRef.children) {
            treeNode.dataRef.children = []
          }
          if (res.directories.length > 0) {
            const d = res.directories
            d.forEach((item, index, d) => {
              item.type = 'dir'
            })
            treeNode.dataRef.children = d
          }
          if (res.files.length > 0) {
            const a = res.files
            a.forEach((item, index, a) => {
              item.isLeaf = true
              item.type = 'file'
            })
            treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
          }

          this.treeData = [...this.treeData]
          resolve()
        })
      })
    },
    treeSelect(key, e) {
      this.logs = ""
      this.path = ""
      let nodeData = e.node.dataRef
      if (nodeData && nodeData.type === 'file') {
        this.path = nodeData.path
        this.viewLogs()
      }
    },
  },

};
</script>

<style lang="scss" scoped>

</style>