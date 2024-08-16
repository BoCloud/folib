<template>
  <a-row type="flex" :gutter="24">
    <a-col :span="24" :md="6" class="mb-24">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
        <template #title>
          <span class="font-semibold m-0">{{ $t('Setting.artifactRepository') }}</span>
          <span class="font-regular mr-20" style="float: right;color: darkgrey">{{repositoryUsed}}/{{repositoryTotal}}</span>
        </template>
        <ul class="list settings-list">
          <li v-for="item in repositoryEvents" :key="item.id">
            <a-switch v-model="item.checked" @change="updateEvent(item)"/>
            <span>{{ item.eventName }}</span>
          </li>
        </ul>
      </a-card>
    </a-col>
    <a-col :span="24" :md="6" class="mb-24">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
        <template #title>
          <span class="font-semibold m-0">{{ $t('Setting.systemSetting') }}</span>
          <span class="font-regular mr-20" style="float: right;color: darkgrey">{{systemUsed}}/{{systemTotal}}</span>
        </template>
        <ul class="list settings-list">
          <li v-for="item in systemEvents" :key="item.id">
            <a-switch v-model="item.checked" @change="updateEvent(item)"/>
            <span>{{ item.eventName }}</span>
          </li>
        </ul>
      </a-card>
    </a-col>
    <a-col :span="24" :md="6" class="mb-24">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
        <template #title>
          <span class="font-semibold m-0">{{ $t('Setting.advanceSetting') }}</span>
          <span class="font-regular mr-20" style="float: right;color: darkgrey">{{advancedUsed}}/{{advancedTotal}}</span>
        </template>
        <ul class="list settings-list">
          <li v-for="item in advanceEvents" :key="item.id">
            <a-switch v-model="item.checked" @change="updateEvent(item)"/>
            <span>{{ item.eventName }}</span>
          </li>
        </ul>
      </a-card>
    </a-col>
    <a-col :span="24" :md="6" class="mb-24">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
        <template #title>
          <span class="font-semibold m-0">{{ $t('Setting.userManagement') }}</span>
          <span class="font-regular mr-20" style="float: right;color: darkgrey">{{userUsed}}/{{userTotal}}</span>
        </template>
        <ul class="list settings-list">
          <li v-for="item in userEvents" :key="item.id">
            <a-switch v-model="item.checked" @change="updateEvent(item)"/>
            <span>{{ item.eventName }}</span>
          </li>
        </ul>
      </a-card>
    </a-col>
  </a-row>
</template>

<script>
import {getEvents, updateEvent} from "@/api/audit"

export default {
  name: "AuditConfig.vue",
  data() {
    return {
      allEvents: [],
      groupedData: {},
      repositoryEvents: [],
      userEvents: [],
      systemEvents: [],
      advanceEvents: [],
      model: true,
    }
  },
  methods: {
    getALlEvents() {
      getEvents().then(res => {
        this.allEvents = res;
        const groupedData={};
        this.allEvents.forEach(item => {
          item.checked = !!item.used
          const moduleValue = item.moduleValue;
          if (!groupedData[moduleValue]) {
            groupedData[moduleValue] = [];
          }
          groupedData[moduleValue].push(item);
        });
        this.repositoryEvents =groupedData['ARTIFACT_REPOSITORY'];
        this.userEvents = groupedData['USER_MANAGEMENT'];
        this.systemEvents =groupedData['SYSTEM_SETTING'];
        this.advanceEvents =groupedData['ADVANCE_SETTING'];
      })
    },
    updateEvent(item) {
      item.used = item.checked ? 1 : 0;
      updateEvent(item).then(res => {
        this.getALlEvents();
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      });
    },
  },
  computed: {

    repositoryTotal(){
      return this.repositoryEvents.length;
    },
    repositoryUsed(){
      return this.repositoryEvents.filter(item => item.used === 1).length;
    },
    systemTotal(){
      return this.systemEvents.length;
    },
    systemUsed(){
      return this.systemEvents.filter(item => item.used === 1).length;
    },

    advancedTotal(){
      return this.advanceEvents.length;
    },
    advancedUsed(){
      return this.advanceEvents.filter(item => item.used === 1).length;
    },
    userTotal(){
      return this.userEvents.length;

    },
    userUsed(){
      return this.userEvents.filter(item => item.used === 1).length;
    },
  },

  mounted() {
      this.getALlEvents();
    }
  }
</script>

<style scoped>

</style>