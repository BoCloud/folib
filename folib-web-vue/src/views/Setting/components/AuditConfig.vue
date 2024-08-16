<template>
  <a-row type="flex" :gutter="24">
    <a-col :span="24" :md="6" class="mb-24">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
        <template #title>
          <h6 class="font-semibold m-0">{{ $t('Setting.artifactRepository') }}</h6>
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
          <h6 class="font-semibold m-0">{{ $t('Setting.systemSetting') }}</h6>
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
          <h6 class="font-semibold m-0">{{ $t('Setting.advanceSetting') }}</h6>
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
          <h6 class="font-semibold m-0">{{ $t('Setting.userManagement') }}</h6>
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
    mounted() {
      this.getALlEvents();
    }
  }
</script>

<style scoped>

</style>