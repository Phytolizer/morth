#pragma once

#include <vector>

template <typename T, typename Container = std::vector<T>> class RandomAccessStack {
    Container m_container;

  public:
    RandomAccessStack() = default;

    inline void push(T value) {
        m_container.push_back(std::move(value));
    }

    inline void emplace(T&& value) {
        m_container.emplace_back(std::move(value));
    }

    inline T pop() {
        T value = std::move(m_container.back());
        m_container.pop_back();
        return value;
    }

    inline T& top() {
        return m_container.back();
    }

    inline const T& top() const {
        return m_container.back();
    }

    inline T& operator[](std::size_t index) {
        return m_container[index];
    }

    inline const T& operator[](std::size_t index) const {
        return m_container[index];
    }

    inline std::size_t size() const {
        return m_container.size();
    }
};
