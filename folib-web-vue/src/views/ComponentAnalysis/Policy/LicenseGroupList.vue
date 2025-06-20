<template>
  <div>
    <div class="mx-25 search">
      <div style="display: flex; justify-content: space-between">
        <a-button type="primary" icon="plus" @click="handleAdd"> 新增证书组 </a-button>
        <a-input-search placeholder="输入证书名称查询" class="v-search" v-model="queryParams.searchText" @search="handheTableSearch()" />
      </div>
    </div>
    <a-table
      rowKey="uuid"
      :columns="columns"
      :data-source="data"
      @change="handleChangeTable"
      :scroll="{ x: true }"
      :pagination="{ pageSize: queryParams.pageSize, current: queryParams.pageNumber, total: queryParams.total, showLessItems: true }"
    >
      <!-- <a slot="action" slot-scope="text" href="javascript:;">Delete</a> -->
      <div slot="expandedRowRender" slot-scope="record" style="margin: 0">
        <div class="record-name">
          <a-form :form="form">
            <a-form-item label="名称">
              <a-input v-model="record.name" @change="handleChangeName(record)" placeholder="名称" class="add-name" />
            </a-form-item>
          </a-form>
        </div>
        <div class="record-licenses">
          <p>证书</p>
          <div class="bar-data" v-for="item in record.licenses" :key="item.uuid">
            <p v-if="record.licenses">{{ item.name }}</p>
            <a-icon type="delete" theme="twoTone" two-tone-color="#f86c6b" v-if="record.licenses" @click="handleDeleteLicence(record, item)" />
          </div>
          <div class="bar-add">
            <p></p>
            <a-icon type="plus-square" theme="twoTone" @click="handleAddLicense(record)" />
          </div>
          <div class="bar-del">
            <a-button type="danger" @click="handleClickDel(record.uuid)"> 删除证书组 </a-button>
          </div>
        </div>
      </div>
    </a-table>
    <!-- 证书组 -->
    <a-drawer title="新增证书组" placement="right" :closable="false" :visible="visible" @close="onClose" :width="720">
      <a-form :model="addForm" :rules="addRules" ref="addForm">
        <a-form-item label="名称" prop="name">
          <a-input
            v-model="addForm.name"
            placeholder="名称"
            v-decorator="['name', { rules: [{ required: true, message: 'Please input your note!' }] }]"
          />
        </a-form-item>
      </a-form>
      <div :style="{}" class="drawer-inner">
        <a-button :style="{ marginRight: '8px' }" @click="onClose"> 取消 </a-button>
        <a-button type="primary" @click="handleSureAdd"> 提交 </a-button>
      </div>
    </a-drawer>
    <!-- 证书 -->
    <a-drawer title="选择证书" placement="right" :closable="false" :visible="licenseVisible" @close="onClose" :width="720">
      <div class="mx-25 search">
        <div style="display: flex; justify-content: space-between">
          <a-button type="primary" icon="plus" @click="handleAdd"> 新增证书组 </a-button>
          <a-input-search
            placeholder="输入证书名称查询"
            class="v-search"
            v-model="queryParams1.searchText"
            @search="handheTableSearch1()"
            @change="handleChangeSearch"
          />
        </div>
      </div>

      <a-table
        rowKey="uuid"
        :columns="columns1"
        :data-source="data1"
        @change="handleChangeTable1"
        :scroll="{ x: true }"
        :row-selection="{ onChange: onSelectChange, selectedRowKeys: selectedRowKeys }"
        :pagination="{ pageSize: queryParams1.pageSize, current: queryParams1.pageNumber, total: queryParams1.total, showLessItems: true }"
      >
      </a-table>

      <div :style="{}" class="drawer-inner">
        <a-button :style="{ marginRight: '8px' }" @click="onCloseSelect"> 取消 </a-button>
        <a-button type="primary" @click="handleSureSelect"> 提交 </a-button>
      </div>
    </a-drawer>
  </div>
</template>

<script>
import { getLicenseGroup, editLicenseGroup, deleteLicenseGroup, addLicenseGroup, getLicense, selectLicense, deleteLicense } from "@/api/policy.js";
export default {
  data() {
    return {
      columns: [{ title: "", dataIndex: "name", key: "name" }],
      data: [],
      columns1: [
        { title: "名称", dataIndex: "name", key: "name" },
        { title: "证书ID", dataIndex: "licenseId", key: "licenseId" },
      ],
      data1: [],
      queryParams: {
        pageNumber: 1,
        pageSize: 10,
        total: 0,
        searchText: "",
      },
      queryParams1: {
        pageNumber: 1,
        pageSize: 10,
        total: 0,
        searchText: "",
      },
      form: this.$form.createForm(this, { name: "form" }),
      addForm: {
        name: "",
      },
      addRules: {
        name: [
          {
            required: true,
            message: "请输入名称",
            trigger: "change",
          },
        ],
      },
      visible: false,
      licenseVisible: false,
      licenseGroupUuid: "",
      selectedRowKeys: [],
    };
  },
  created() {
    this.getData();
  },
  methods: {
    getData() {
      getLicenseGroup(this.queryParams).then((res) => {
        this.queryParams.total = +res.headers["x-total-count"];
        this.data = res.data;
      });
    },
    getLicenseData() {
      getLicense(this.queryParams1).then((res) => {
        // this.queryParams1.total = +res.headers["x-total-count"];
        this.queryParams1.total = res.data.length;
        this.data1 = res.data;
      });
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.pageNumber = pagination.current;
      }
      this.queryParams.sortName = sorter.field;
      if (sorter && sorter.order === "descend") {
        this.queryParams.sortOrder = "desc";
      } else if (sorter && sorter.order === "ascend") {
        this.queryParams.sortOrder = "asc";
      } else {
        this.queryParams.sortOrder = "";
      }
      this.getData();
    },
    handleChangeTable1(pagination, filters, sorter) {
      this.queryParams1.pageNumber = pagination.current;
    },
    handheTableSearch() {
      this.queryParams.pageNumber = 1;
      this.getData();
    },
    handheTableSearch1() {
      this.queryParams1.pageNumber = 1;
      if (this.queryParams1.searchText == "") {
        this.getLicenseData();
      } else {
        this.data1 = this.data1.filter((item) => {
          const isHas = item.name.indexOf(this.queryParams1.searchText);
          if (isHas !== -1) {
            return item;
          }
        });
      }
    },
    handleChangeSearch(val) {
      if (val.target.value == "") {
        this.getLicenseData();
      }
    },
    // 修改证书组名称
    handleChangeName(val) {
      const params = {
        uuid: val.uuid,
        name: val.name,
      };
      editLicenseGroup(params).then((res) => {
        // console.log(res);
        this.$notification["success"]({
          message: "修改成功",
        });
      });
    },
    // 删除证书组
    handleClickDel(val) {
      deleteLicenseGroup(val).then((res) => {
        this.$notification["success"]({
          message: "删除成功",
        });
        this.getData();
      });
    },
    // 删除单个证书
    handleDeleteLicence(record, item) {
      const recordId = record.uuid;
      const licenceId = item.uuid;
      deleteLicense(recordId, licenceId).then((res) => {
        this.getData();
      });
    },
    handleAdd() {
      this.visible = true;
    },
    onClose() {
      this.visible = false;
    },
    onCloseSelect() {
      this.licenseVisible = false;
      this.selectedRowKeys = [];
    },
    // 确认证书组新增
    handleSureAdd() {
      addLicenseGroup(this.addForm).then((res) => {
        if (res && res.status === 201) {
          this.$notification["success"]({
            message: "新增成功",
          });
          this.visible = false;
        } else {
          this.$notification["error"]({
            message: "新增失败",
          });
        }
        this.getData();
        // console.log(res);
      });
    },
    //   新增证书
    handleAddLicense(record) {
      this.licenseGroupUuid = record.uuid;
      this.licenseVisible = true;
      this.getLicenseData();
    },
    handleSureSelect() {
      this.selectedRowKeys.forEach((item) => {
        selectLicense(this.licenseGroupUuid, item).then((res) => {
          this.licenseVisible = false;
          this.selectedRowKeys = [];
          this.getData();
        });
      });
    },
    onSelectChange(selectedRowKeys) {
      this.selectedRowKeys = selectedRowKeys;
    },
  },
};
</script>

<style lang="scss" scoped>
.record-licenses {
  width: 100%;
  margin-top: 20px;
  padding-right: 30px;

  .record-name {
    width: 100%;
  }
  .bar-data {
    display: flex;
    border: 1px solid #eee;
    align-items: center;
    padding: 0 12px;
    margin-bottom: 1px;

    p {
      flex: 1;
      margin: 0;
      line-height: 38px;
    }
    i {
      font-size: 20px;
      cursor: pointer;
    }
  }
  .bar-add {
    display: flex;
    align-items: center;
    padding: 0 12px;
    border: 1px solid #eee;

    p {
      flex: 1;
      margin: 0;
      height: 38px;
      line-height: 38px;
    }
    i {
      font-size: 20px;
      cursor: pointer;
    }
  }
  .bar-del {
    margin-top: 20px;
    float: right;
  }
}

// ::v-deep .ant-input {
//   width: 300px !important;
// }

.search {
  width: 98%;
  height: 50px;
}
.mx-25 .ant-row-flex {
  flex-wrap: wrap;
}
.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}
.add-name {
  max-width: 300px;
  width: 300px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}

.drawer-inner {
  position: absolute;
  right: 0;
  bottom: 0;
  width: 100%;
  border-top: 1px solid #e9e9e9;
  padding: 10px 16px;
  background: #fff;
  text-align: right;
  z-index: 1;
}
</style>
