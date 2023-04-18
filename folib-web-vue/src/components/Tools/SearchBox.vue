<template>
  <form onsubmit="event.preventDefault();" role="search" @mouseover="mouse(true)" @mouseleave="mouse(false)">
    <label for="search">Search for stuff</label>
    <input id="search" type="search" placeholder="搜索制品..." required v-model="value" />
    <a-dropdown>
      <a-menu slot="overlay" @click="handleSearchMenuClick">
        <a-menu-item :key="1">
          普通
        </a-menu-item>
        <a-menu-item :key="2">
          元数据
        </a-menu-item>
      </a-menu>
      <a-button> 维度 <a-icon type="down" /> </a-button>
    </a-dropdown>
  </form>
</template>

<script>
export default {
  data() {
    return {
      value: '',
      searchType: 1,
      hover: false
    }
  },
  methods: {
    search() {
      this.$emit('search', this.value, this.searchType, 1)
    },
    handleSearchMenuClick(active) {
      this.searchType = active.key
      this.search()
    },
    mouse(bool) {
      this.$emit('mouse', bool)
    }
  }
}
</script>
<style lang="scss" scoped>
$rad: 60px;
$dur: 0.3s;
$colorDark: #2f2f2f;
$colorLight: rgba(255, 255, 255, 1);
$colorBrand: #aeb0b9;
$ff: 'Lato', sans-serif;
$h: 50px;
$btnWidth: 100px;
$fs: 17px;
$bez: cubic-bezier(0, 0, 0.43, 1.49);

// Setup
body {
  background: $colorDark;
  display: flex;
  align-items: center;
  justify-content: center;
}

html {
  box-sizing: border-box;
  height: 100%;
  font-size: 10px;
}

*,
*::before,
*::after {
  box-sizing: inherit;
}

// Main styles
form {
  position: relative;
  width: 30rem;
  background: $colorBrand;
  border-radius: $rad;
  opacity: 0.3;
  transition: all 1s;

  &:hover {
    opacity: 1;
  }
}

input,
button {
  height: $h;
  font-family: $ff;
  border: 0;
  color: $colorDark;
  font-size: $fs;
}

input[type='search'] {
  outline: 0; // <-- shold probably remove this for better accessibility, adding for demo aesthetics for now.
  width: 100%;
  background: $colorLight;
  padding: 0 30px;
  border-radius: $rad;
  appearance: none; //for iOS input[type="search"] roundedness issue. border-radius alone doesn't work
  transition: all $dur $bez;
  transition-property: width, border-radius;
  z-index: 1;
  position: relative;
  box-shadow: 0 0 15px #908585;
}

button {
  display: none; // prevent being able to tab to it
  position: absolute;
  top: 0;
  right: 0;
  width: $btnWidth;
  font-weight: bold;
  background: $colorBrand;
  border-radius: 0 $rad $rad 0;
}

input:not(:placeholder-shown) {
  border-radius: $rad 0 0 $rad;
  width: calc(100% - 90px);

  +button {
    display: block;
    color: $colorLight;
    cursor: pointer;
  }
}

label {
  position: absolute;
  clip: rect(1px, 1px, 1px, 1px);
  padding: 0;
  border: 0;
  height: 1px;
  width: 1px;
  overflow: hidden;
}
</style>
