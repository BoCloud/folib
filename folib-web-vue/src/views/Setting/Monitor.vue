<template>
  <div id="settings">
    <a-tabs class="tabs-sliding" v-model="activeTab" default-active-key="1" >

      <a-tab-pane key="1" :tab="$t('Setting.BaseMonitoring')">
      <base-data  :activeTab="activeTab"/>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Setting.SystemLog')">
    <system-log />
      </a-tab-pane>
      <a-tab-pane key="3" :tab="$t('Setting.dataQuery')">
        <query-data :navbar-fixed="navbarFixed"/>
      </a-tab-pane>
      <a-tab-pane key="4" :tab="$t('Setting.clusterInformation')" class="cluster">
             <cluster-info v-if="activeTab==='4'" />
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
<script>
import QueryData from './components/QueryData';
import BaseData from './components/BaseData';
import SystemLog from './components/SystemLog';
import ClusterInfo from './components/ClusterInfo';

export default {
  props: ['navbarFixed'],
  data() {
    return {
      activeTab:'1',

    };
  },
  components: {
    QueryData,
    BaseData,
    SystemLog,
    ClusterInfo
  },
  created() {

  },
  mounted() {
  },


};
</script>

<style lang="scss" scoped>
#settings::v-deep {
  .my-editor {
    background: #171717;
    color: #b7b6b6;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .my-editor3 {
    background: #030303;
    color: #e8e8e8;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .my-editor2 {
    background: #fafafa;
    color: #595959;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .g6-x {
    width: calc(100% - 10px);
    height: 500px;
    box-sizing: border-box;
    margin-left: 20px;
  }

  .ant-list {
    width: 100%;
  }

  .ant-list-item-meta-avatar {
    margin-right: 8px;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-ink::before {
    display: none;
  }

  .ant-anchor-link {
    padding: 0;
    margin-top: 8px;
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    background-color: #eeeeee;
  }

  .ant-anchor-link a svg g {
    fill: #344767;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .card-profile-head {
    margin: 0 0 24px;
  }

  .tags-field .ant-form-item-control {
    line-height: 33px;
  }

  .form-tag.ant-tag {
    border-radius: 20px;
    padding: 4px 10px;
    font-size: 12px;
    font-weight: 500;
    margin-right: 3.75px;
    margin-bottom: 3.75px;
    background-color: #3a416f;
    border: 1px solid #3a416f;
    color: #fff;
  }

  .form-tag.ant-tag .anticon-close {
    color: #fff;
    height: 16px;
    border-left: 1px solid hsla(0, 0%, 100%, .3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: .75;
  }

  .form-tag.ant-tag .anticon-close:hover {
    color: #fff;
    opacity: 1;
  }

  .tags-field .ant-input {
    margin-bottom: 5px;
    margin-top: 4px;
  }

  .tags-field .ant-select {
    .ant-select-selection__choice__remove i {
      color: #fff;
      height: 16px;
      border-left: 1px solid hsla(0, 0%, 100%, .3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: .75;

      &:hover {
        color: #fff;
        opacity: 1;
      }
    }

    .ant-select-selection__rendered>ul>li:not(.ant-select-search) {
      border-radius: 20px;
      padding: 2px 27px 2px 10px;
      font-size: 12px;
      font-weight: 500;
      margin-right: 3.75px;
      margin-bottom: 3.75px;
      background-color: #3a416f;
      border: 1px solid #3a416f;
      color: #fff;
      line-height: 2;
      height: 30px;
    }

    .ant-select-selection--multiple {
      padding: 8px 10px;
    }
  }

  .cluster .ant-avatar img {
    height: unset;
  }

  .cluster .current-node {
    border: 3px solid #ddd;
    border-image: linear-gradient(#A18DFF, #005CFD) 1;
    clip-path: inset(0 round 6px);
  }

  .cluster .remove-node-popconfirm {
    margin: 0 16px 0 0;
    cursor: pointer;
  }
}
</style>
