---
title: Projects
layout: collection
permalink: /projects/
collection: portfolio
homeex: true
entries_layout: grid
---

<h2>My Projects</h2>
<ul>
  {% for project in site.portfolio %}
    <li>
      <strong>{{ project.title }}</strong> - 
      <a href="{{ project.url | relative_url }}">View Details</a>
      
      {% if project.github_repo %}
        | <a href="https://github.com/{{ project.github_repo }}" target="_blank" rel="noopener">
            View on GitHub
          </a>
      {% endif %}
    </li>
  {% endfor %}
</ul>
